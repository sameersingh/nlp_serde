package org.sameersingh.nlp_serde

import org.junit.Test
import org.sameersingh.nlp_serde.annotators.{AnnotatorPipeline, StanfordAnnotator}
import org.sameersingh.nlp_serde.annotators.relations.PatternAnnotator

/**
 * @author sameer
 * @since 9/21/14.
 */
class PatternExtractorTest {
  def sampleDoc = {
    val d = new Document
    d.id = "doc001"
    d.text = "Barack Obama, the president of USA, was born in Hawaii. He criticized Nehru"
    d
  }

  @Test
  def testAnnotations() {
    val d = sampleDoc
    val annotator = new AnnotatorPipeline(Seq(new StanfordAnnotator(), new PatternAnnotator()))
    val pd = annotator.process(d)
    println(pd)
  }

}
