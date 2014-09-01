package org.sameersingh.nlp_serde.annotators

import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import java.util.Properties
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.ling.CoreLabel
import edu.stanford.nlp.util.CoreMap
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation
import edu.stanford.nlp.dcoref.CorefChain
import edu.stanford.nlp.dcoref.CorefCoreAnnotations.CorefChainAnnotation
import org.sameersingh.nlp_serde._
import scala.collection.JavaConversions._
import scala.Some

/**
 * @author sameer
 * @since 9/1/14.
 */
class StanfordAnnotator(val annotators: Seq[String] = Seq("tokenize", "ssplit", "pos", "lemma", "ner", "parse", "dcoref"))
  extends Annotator {
  // creates a StanfordCoreNLP object, with POS tagging, lemmatization, NER, parsing, and coreference resolution
  val props = new Properties()
  props.put("annotators", annotators.mkString(", "))
  val pipeline = new StanfordCoreNLP(props)

  def printCoreMap(c: CoreMap) {
    println(c.toShorterString())
    //println(c.keySet().map(k => k -> c.get(k)).mkString("\t", "\n\t", "\n"))
  }

  override def process(doc: Document): Document = {
    val text = doc.text

    // create an empty Annotation just with the given text
    val document = new Annotation(text)

    // run all Annotators on this text
    pipeline.annotate(document)
    //println("doc: " + document)
    //printCoreMap(document)

    // these are all the sentences in this document
    // a CoreMap is essentially a Map that uses class objects as keys and has values with custom types
    val sentences = document.get(classOf[SentencesAnnotation])

    for (sentence <- sentences) {
      //println("sen: " + sentence)
      //printCoreMap(sentence)
      val s = new Sentence()
      s.idx = doc.sentences.size
      s.chars = sentence.get(classOf[CharacterOffsetBeginAnnotation]).toInt -> sentence.get(classOf[CharacterOffsetEndAnnotation]).toInt
      s.text = sentence.get(classOf[TextAnnotation])
      for (token: CoreLabel <- sentence.get(classOf[TokensAnnotation])) {
        //println("tok: " + token)
        //printCoreMap(token)
        val t = new Token()
        t.idx = token.sentIndex()
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
      val dependencies = sentence.get(classOf[CollapsedCCProcessedDependenciesAnnotation])
      s.depTree = Some(dependencies.toString)

      doc.sentences += s
    }

    // This is the coreference link graph
    // Each chain stores a set of mentions that link to each other,
    // along with a method for getting the most representative mention
    // Both sentence and token offsets start at 1!
    val graph = document.get(classOf[CorefChainAnnotation])
    for ((id, chain) <- graph) {
      val e = new Entity()
      e.id = id.toInt
      val rep = chain.getRepresentativeMention
      for(mention <- chain.getMentionsInTextualOrder) {
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
        if(rep.position == mention.position) {
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
