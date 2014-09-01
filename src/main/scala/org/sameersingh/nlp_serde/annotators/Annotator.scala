package org.sameersingh.nlp_serde.annotators

import org.sameersingh.nlp_serde.Document

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
