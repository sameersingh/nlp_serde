package org.sameersingh.nlp_serde.writers

import org.sameersingh.nlp_serde.{JsonUtil, Document}
import java.io.PrintWriter
import play.api.libs.json.Json

/**
 * Json writer that writes a single json per file
 * @author sameer
 * @since 9/1/14.
 */
class JsonWriter extends Writer with DocPerFile {
  override def writeDoc(name: String, doc: Document) {
    val writer = new PrintWriter(name)
    writer.println(Json.prettyPrint(JsonUtil.fromDoc(doc.toCase)))
    writer.flush
    writer.close
  }
}

/**
 * Json writer that writes a single json per line
 * @author sameer
 * @since 9/1/14.
 */
class PerLineJsonWriter extends Writer {
  override def write(name: String, docs: Iterator[Document]) {
    val writer = new PrintWriter(name)
    for (doc <- docs)
      writer.println(Json.prettyPrint(JsonUtil.fromDoc(doc.toCase)))
    writer.flush
    writer.close
  }
}