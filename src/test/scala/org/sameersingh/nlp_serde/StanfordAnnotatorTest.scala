package org.sameersingh.nlp_serde

import org.junit.Test
import org.sameersingh.nlp_serde.annotators.StanfordAnnotator

/**
 * @author sameer
 * @since 9/1/14.
 */
class StanfordAnnotatorTest {
  def sampleDoc = {
    val d = new Document
    d.id = "doc001"
    d.text = "Barack Obama was born in Hawaii. He is the president of USA."
    d
  }

  @Test
  def testAnnotations() {
    val d = sampleDoc
    val annotator = new StanfordAnnotator()
    val pd = annotator.process(d)
    println(pd)
  }
}
