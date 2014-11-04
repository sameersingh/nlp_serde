package nlp_serde.writers

import nlp_serde.Document

/**
 * @author sameer
 * @since 9/1/14.
 */
trait Writer {
  def write(name: String, docs: Iterator[Document]): Unit
}

// exactly one document per file
trait DocPerFile extends Writer {
  def writeDoc(name: String, doc: Document): Unit

  override def write(name: String, docs: Iterator[Document]) {
    docs.zipWithIndex.foreach(di => writeDoc(name + "." + di._2, di._1))
  }
}
