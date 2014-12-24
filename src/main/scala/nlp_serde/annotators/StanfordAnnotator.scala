package nlp_serde.annotators

import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import java.util.Properties
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.{BasicDependenciesAnnotation, CollapsedCCProcessedDependenciesAnnotation}
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
import nlp_serde._
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.PerLineJsonWriter
import scala.collection.JavaConversions._
import edu.stanford.nlp.semgraph.SemanticGraph
import nlp_serde.immutable.Dep

/**
 * @author sameer
 * @since 9/1/14.
 */
class StanfordAnnotator(val annotators: Seq[String] = Seq("tokenize", "ssplit", "pos", "lemma", "ner", "parse", "dcoref"), val collapsed:Boolean = true)
  extends Annotator {
  // creates a StanfordCoreNLP object, with POS tagging, lemmatization, NER, parsing, and coreference resolution
  val props = new Properties()
  props.put("annotators", annotators.mkString(", "))
  val pipeline = new StanfordCoreNLP(props)

  def depTreeFromSemanticG(graph: SemanticGraph): Seq[Dep] = {
    (graph.getRoots().map(e => Dep("root", 0, e.index()))
      ++ graph.edgeIterable().map(e => Dep(e.getRelation.toString, e.getSource.index(), e.getTarget.index()))).toSeq
  }

  override def process(doc: Document): Document = {
    val text = doc.text
    val document = new Annotation(text)
    pipeline.annotate(document)

    val sentences = document.get(classOf[SentencesAnnotation])

    for (sentence <- sentences) {
      val s = new Sentence()
      s.idx = doc.sentences.size
      s.chars = sentence.get(classOf[CharacterOffsetBeginAnnotation]).toInt -> sentence.get(classOf[CharacterOffsetEndAnnotation]).toInt
      s.text = sentence.get(classOf[TextAnnotation])
      for (token: CoreLabel <- sentence.get(classOf[TokensAnnotation])) {
        val t = new Token()
        t.idx = token.index()
        t.chars = token.beginPosition() -> token.endPosition()
        // t.text = token.get(classOf[TextAnnotation])
        t.text = token.originalText()
        t.lemma = Some(token.lemma())
        val stem = token.get(classOf[StemAnnotation])
        t.pos = Some(token.get(classOf[PartOfSpeechAnnotation]))
        t.ner = Some(token.ner())
        s.tokens += t
      }

      // this is the parse tree of the current sentence
      val tree = sentence.get(classOf[TreeAnnotation])
      s.parseTree = Some(tree.toString)
      // this is the Stanford dependency graph of the current sentence
      val dependencies = {
        if (collapsed)
          sentence.get(classOf[CollapsedCCProcessedDependenciesAnnotation])
        else
          sentence.get(classOf[BasicDependenciesAnnotation])
      }
      s.depTree = Some(depTreeFromSemanticG(dependencies))
      doc.sentences += s
    }

    val graph = document.get(classOf[CorefChainAnnotation])
    for ((id, chain) <- graph) {
      val e = new Entity()
      e.id = id.toInt
      val rep = chain.getRepresentativeMention
      for (mention <- chain.getMentionsInTextualOrder) {
        val m = new Mention()
        m.entityId = Some(e.id)
        m.id = mention.mentionID
        m.headTokenIdx = mention.headIndex
        m.mentionType = Some(mention.mentionType.toString)
        m.sentenceId = mention.sentNum
        m.posInSentence = mention.position.get(1)
        m.text = mention.mentionSpan
        m.toks = mention.startIndex -> mention.endIndex
        val headToken = doc.sentences(m.sentenceId - 1).tokens(m.headTokenIdx - 1)
        m.ner = headToken.ner
        m.attrs("GENDER") = mention.gender.toString
        m.attrs("ANIMACY") = mention.animacy.toString
        m.attrs("NUMBER") = mention.number.toString
        doc.sentences(m.sentenceId - 1).mentions += m
        e.mids += m.id
        if (rep.position == mention.position) {
          e.representativeMId = m.id
          e.representativeString = m.text
          e.ner = m.ner
        }
      }
      doc.entities += e
    }
    doc
  }
}

object StanfordAnnotator {
  def main(args: Array[String]): Unit = {
    if(args.length!= 2) {
      println("Not enough arguments: input_file.json.gz output_file.json.gz")
    }
    val input = args(0)
    val output = args(1)
    println("Reading: " + input)
    val reader = new PerLineJsonReader(true)
    val docs = reader.read(input)
    val annotator = new StanfordAnnotator()
    val nlpDocs = annotator.process(docs)
    println("Writing: " + output)
    val writer = new PerLineJsonWriter(true)
    writer.write(output, nlpDocs)
  }
}
