package nlp_serde.readers

import nlp_serde.{JsonUtil, FileUtil, Document}
import nlp_serde.immutable
import play.api.libs.json._

/**
 * Json reader where each file contains the json for a single document
 * @author sameer
 * @since 9/1/14.
 */
class JsonReader(gzip: Boolean = true) extends Reader with DocPerFile {
  override def readDoc(name: String): Document = {
    val source = FileUtil.inputSource(name, gzip)
    val json = source.getLines().mkString("\n")
    new Document(JsonUtil.toDoc(json))
  }
}

/**
 * Json reader where each file contains the json for multiple documents (one json/doc per line)
 * @author sameer
 * @since 9/1/14.
 */
class PerLineJsonReader(gzip: Boolean = true) extends Reader with DocsPerFile {
  override def read(name: String): Iterator[Document] = {
    FileUtil.inputSource(name, gzip).getLines().map(str => {
      new Document(JsonUtil.toDoc(str))
    })
  }
}
