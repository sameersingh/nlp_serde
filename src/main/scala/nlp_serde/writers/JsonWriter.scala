package nlp_serde.writers

import nlp_serde.readers.PerLineJsonReader
import nlp_serde.{JsonUtil, FileUtil, Document}
import nlp_serde.JsonUtil
import java.io.{File, PrintWriter}
import play.api.libs.json.Json

import scala.collection.mutable

/**
 * Json writer that writes a single json per file
 * @author sameer
 * @since 9/1/14.
 */
class JsonWriter(gzip: Boolean = true) extends Writer with DocPerFile {
  override def writeDoc(name: String, doc: Document) {
    val writer = FileUtil.writer(name + ".json" + (if (gzip) ".gz" else ""), gzip)
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

/**
 * Write out multiple files, splitting the input collection of documents in multiple ones
 */
class SplitPerLineJsonWriter(val fname: (File, Document) => String, val gzip: Boolean = true) extends Writer {
  override def write(name: String, docs: Iterator[Document]): Unit = {
    val writers = new mutable.HashMap[String, PrintWriter]()
    for (d <- docs) {
      val newName = fname(new File(name), d)
      val writer = writers.getOrElseUpdate(newName, FileUtil.writer(newName, gzip))
      writer.println(Json.stringify(JsonUtil.fromDoc(d.toCase)))
    }
    writers.values.foreach(w => {
      w.flush()
      w.close()
    })
  }
}

object SplitPerLineJsonWriter {
  def main(args: Array[String]): Unit = {
    assert(args.length == 1)
    val inputFile = args(0)
    def newName(file: File, d: Document): String = {
      val name = file.getName
      val parent = file.getParent
      val year = d.attrs.get("date").getOrElse("NONE").take(4)
      "%s/%s.%s".format(parent, year, name)
    }
    val docs = new PerLineJsonReader(true).read(inputFile)
    println(docs.size)
    //val writer = new SplitPerLineJsonWriter(newName, true)
    //writer.write(inputFile, docs)
  }
}