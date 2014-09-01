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
import org.sameersingh.nlp_serde.Document

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

  override def process(doc: Document): Document = {
    val text = doc.text

    // create an empty Annotation just with the given text
    val document = new Annotation(text)

    // run all Annotators on this text
    pipeline.annotate(document)

    // these are all the sentences in this document
    // a CoreMap is essentially a Map that uses class objects as keys and has values with custom types
    val sentences = document.get(classOf[SentencesAnnotation])

    for (sentence: CoreMap <- sentences) {
      // traversing the words in the current sentence
      // a CoreLabel is a CoreMap with additional token-specific methods
      for (token: CoreLabel <- sentence.get(classOf[TokensAnnotation])) {
        // this is the text of the token
        val word = token.get(classOf[TextAnnotation])
        // this is the lemma of the token
        val lem = token.get(classOf[LemmaAnnotation])
        // this is the stem of the token
        val stem = token.get(classOf[StemAnnotation])
        // this is the POS tag of the token
        val pos = token.get(classOf[PartOfSpeechAnnotation])
        // this is the NER label of the token
        val ne = token.get(classOf[NamedEntityTagAnnotation])
      }

      // this is the parse tree of the current sentence
      val tree = sentence.get(classOf[TreeAnnotation])

      // this is the Stanford dependency graph of the current sentence
      val dependencies = sentence.get(classOf[CollapsedCCProcessedDependenciesAnnotation])
    }

    // This is the coreference link graph
    // Each chain stores a set of mentions that link to each other,
    // along with a method for getting the most representative mention
    // Both sentence and token offsets start at 1!
    val graph: Map[Integer, CorefChain] = document.get(classOf[CorefChainAnnotation])
    doc
  }
}
