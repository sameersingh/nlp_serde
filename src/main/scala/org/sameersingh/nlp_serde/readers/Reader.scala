package org.sameersingh.nlp_serde.readers

import org.sameersingh.nlp_serde.{FileFilters, Document}
import java.io.{File, FilenameFilter}

/**
 * @author sameer
 * @since 9/1/14.
 */
trait Reader {
  def read(name: String): Iterator[Document]

  def readFilelist(nameListFile: String, lineToName: String => String = identity): Iterator[Document] = {
    (for (l <- io.Source.fromFile(nameListFile, "UTF-8").getLines()) yield {
      read(lineToName(l))
    }).flatten
  }
}

// one or more documents per file
trait DocsPerFile extends Reader {
  def readDir(dir: String, filter: FilenameFilter = FileFilters.all): Iterator[Document] = {
    val file = new File(dir)
    assert(file.exists(), "%s must exist." format (file.getCanonicalPath))
    assert(file.isDirectory, "%s must be a directory." format (file.getCanonicalPath))

    (for (f <- file.listFiles(filter).toIterator) yield {
      read(f.getCanonicalPath)
    }).flatten
  }
}

// exactly one document per file
trait DocPerFile extends DocsPerFile {
  def readDoc(name: String): Document

  override def read(name: String): Iterator[Document] = Iterator(readDoc(name))
}
