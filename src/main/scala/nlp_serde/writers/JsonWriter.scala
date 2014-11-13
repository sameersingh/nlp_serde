package nlp_serde.writers

import nlp_serde.{JsonUtil, FileUtil, Document}
import nlp_serde.JsonUtil
import java.io.PrintWriter
import play.api.libs.json.Json

/**
 * Json writer that writes a single json per file
 * @author sameer
 * @since 9/1/14.
 */
class JsonWriter(gzip: Boolean = true) extends Writer with DocPerFile {
  override def writeDoc(name: String, doc: Document) {
    val writer = FileUtil.writer(name + ".json" + (if(gzip) ".gz" else ""), gzip)
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
class PerLineJsonWriter(gzip: Boolean = true) extends Writer {
  override def write(name: String, docs: Iterator[Document]) {
    val writer = FileUtil.writer(name, gzip)
    for (doc <- docs)
      writer.println(Json.stringify(JsonUtil.fromDoc(doc.toCase)))
    writer.flush
    writer.close
  }
}