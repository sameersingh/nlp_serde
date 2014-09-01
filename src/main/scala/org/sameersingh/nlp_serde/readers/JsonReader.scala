package org.sameersingh.nlp_serde.readers

import org.sameersingh.nlp_serde.{JsonUtil, Document, immutable}
import play.api.libs.json._

/**
 * Json reader where each file contains the json for a single document
 * @author sameer
 * @since 9/1/14.
 */
class JsonReader extends Reader with DocPerFile {
  override def readDoc(name: String): Document = {
    val source = io.Source.fromFile(name, "UTF-8")
    val json = source.getLines().mkString("\n")
    new Document(JsonUtil.toDoc(json))
  }
}

/**
 * Json reader where each file contains the json for multiple documents (one json/doc per line)
 * @author sameer
 * @since 9/1/14.
 */
class PerLineJsonReader extends Reader with DocsPerFile {
  override def read(name: String): Iterator[Document] = {
    io.Source.fromFile(name, "UTF-8").getLines().map(str => {
      new Document(JsonUtil.toDoc(str))
    })
  }
}
