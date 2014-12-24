package nlp_serde.annotators

import nlp_serde.Document

/**
 * Simple class, takes a document, and returns it processed
 * @author sameer
 * @since 9/1/14.
 */
trait Annotator {
  def process(doc: Document): Document

  def process(docs: Iterator[Document]): Iterator[Document] = {
    docs.map(d => process(d))
  }

  def parallelProcess(docs: Iterator[Document], batchSize: Int = 100): Iterator[Document] = {
    docs.grouped(batchSize).map(ds => {
      ds.par.map(d => process(d)).seq
    }).flatten
  }
}

class AnnotatorPipeline(annots: Seq[Annotator]) extends Annotator {
  override def process(doc: Document): Document = annots.foldLeft(doc)((d, a) => a.process(d))
}

class MultiThreadedAnnotator[A <: Annotator](newAnnotator: => A) extends Annotator {
  val batchSize = 1000
  val annotator = newAnnotator
  override def process(docs: Iterator[Document]): Iterator[Document] = {
    println("WARNING: Currently assumes " + annotator.getClass.getSimpleName + ".process() is thread-safe.")
    docs.grouped(batchSize).flatMap(batch => {
      println("Starting batch with %d docs".format(batch.size))
      batch.par.map(d => annotator.process(d))
    })
  }

  override def process(doc: Document): Document = throw new Error("process() should not be called directly")
}