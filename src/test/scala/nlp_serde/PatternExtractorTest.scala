package nlp_serde

import nlp_serde.annotators.relations.PatternAnnotator
import nlp_serde.annotators.{AnnotatorPipeline, StanfordAnnotator}
import org.junit.Test

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
