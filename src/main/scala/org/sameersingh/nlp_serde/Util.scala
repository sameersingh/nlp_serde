package org.sameersingh.nlp_serde

import scala.collection.mutable
import java.io._
import play.api.libs.json._
import play.api.libs.json.JsSuccess
import java.util.zip.{GZIPInputStream, GZIPOutputStream}
import scala.io.BufferedSource

/**
 * @author sameer
 * @since Aug 31 2014
 */
object Util {

  trait Attr {
    val _attrs: mutable.Map[String, String] = new mutable.HashMap[String, String]

    def attrs = _attrs
  }

}

object FileFilters {
  val all = using(s => true)

  def byExtension(suffix: String) = using(s => s.endsWith(suffix))

  def startsWith(prefix: String) = using(s => s.startsWith(prefix))

  def using(f: String => Boolean) = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = f(name)
  }
}

object FileUtil {
  def inputSource(fname: String, gzip: Boolean): BufferedSource = {
    io.Source.fromInputStream(
      if (gzip) new GZIPInputStream(new FileInputStream(fname))
      else new FileInputStream(fname))("UTF-8")
  }

  def writer(fname: String, gzip: Boolean): PrintWriter =
    new PrintWriter(
      if (gzip) new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(fname)), "UTF-8")
      else new FileWriter(fname))
}

object JsonUtil {

  import immutable._

  val intPairWrites: Writes[(Int, Int)] = new Writes[(Int, Int)] {
    override def writes(o: (Int, Int)): JsValue = {
      Json.toJson(Seq(o._1, o._2))
    }
  }
  implicit val intPairWritesImplicit = intPairWrites
  implicit val tokWrites = Json.writes[Token]
  implicit val relWrites = Json.writes[Relation]
  implicit val menWrites = Json.writes[Mention]
  implicit val senwrites = Json.writes[Sentence]
  implicit val entWrites = Json.writes[Entity]
  implicit val docWrites = Json.writes[immutable.Document]

  def fromDoc(doc: immutable.Document): JsValue = {
    Json.toJson(doc)(docWrites)
  }

  val intPairReads: Reads[(Int, Int)] = new Reads[(Int, Int)] {
    override def reads(json: JsValue): JsResult[(Int, Int)] = {
      Json.fromJson[Seq[Int]](json).flatMap(seq => JsSuccess(seq(0) -> seq(1)))
    }
  }
  implicit val intPairReadsImplicit = intPairReads
  implicit val tokReads = Json.reads[Token]
  implicit val relReads = Json.reads[Relation]
  implicit val menReads = Json.reads[Mention]
  implicit val senReads = Json.reads[Sentence]
  implicit val entReads = Json.reads[Entity]
  implicit val docReads = Json.reads[immutable.Document]

  def toDoc(json: String): immutable.Document = {
    Json.fromJson[immutable.Document](Json.parse(json))(docReads).get
  }
}

