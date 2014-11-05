package nlp_serde

import nlp_serde.annotators.StanfordAnnotator
import org.junit.Test

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
