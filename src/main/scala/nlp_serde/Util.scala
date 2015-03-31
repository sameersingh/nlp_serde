package nlp_serde

import java.io._
import java.util.regex.{Matcher, Pattern}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import play.api.libs.json.{JsSuccess, _}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
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

  def indent(num: Int, str: String): String = {
    "%s%s" format((0 until num).map(i => "\t").mkString(""), str)
  }

  def printIndent(num: Int, str: String) {
    println(indent(num, str))
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
  def workDir = java.net.InetAddress.getLocalHost.getHostName match {
    case "earth" => "/home/sameer/work"
    case "sanatarium" => "/Users/sameer/Work"
  }

  def name(path: String): String = new File(path).getName

  def inputSource(fname: String, gzip: Boolean): BufferedSource = {
    io.Source.fromInputStream(
      if (gzip) new GZIPInputStream(new FileInputStream(fname))
      else new FileInputStream(fname))("UTF-8")
  }

  def writer(fname: String, gzip: Boolean): PrintWriter =
    new PrintWriter(
      if (gzip) new OutputStreamWriter(new GZIPOutputStream(new FileOutputStream(fname)), "UTF-8")
      else new FileWriter(fname))

  val unicodePattern = Pattern.compile("\\$....")

  def extractWikiTitle(title: String): String = {
    val str = title.drop(1).dropRight(1)
    val matcher = unicodePattern.matcher(str)
    val patterns = new ArrayBuffer[String]
    while (matcher.find()) {
      patterns += matcher.group().drop(1)
    }
    patterns
      .map(s => s -> new String(Character.toChars(Integer.parseInt(s, 16))))
      .foldLeft(str)((str, pc) => {
      str.replaceAll("\\$" + pc._1, Matcher.quoteReplacement(pc._2))
    })
  }

  def createDir(d: String) = {
    new File(d).mkdirs()
    d
  }
}

object JsonUtil {
  import nlp_serde.immutable._
  val intPairWrites: Writes[(Int, Int)] = new Writes[(Int, Int)] {
    override def writes(o: (Int, Int)): JsValue = {
      Json.toJson(Seq(o._1, o._2))
    }
  }
  implicit val intPairWritesImplicit = intPairWrites
  implicit val tokWrites = Json.writes[Token]
  implicit val relWrites = Json.writes[Relation]
  implicit val menWrites = Json.writes[Mention]
  implicit val depWrites = Json.writes[Dep]
  implicit val senwrites = Json.writes[Sentence]
  implicit val entWrites = Json.writes[Entity]
  implicit val docWrites = Json.writes[nlp_serde.immutable.Document]

  def fromDoc(doc: nlp_serde.immutable.Document): JsValue = {
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
  implicit val depReads = Json.reads[Dep]
  implicit val senReads = Json.reads[Sentence]
  implicit val entReads = Json.reads[Entity]
  implicit val docReads = Json.reads[nlp_serde.immutable.Document]

  def toDoc(json: String): nlp_serde.immutable.Document = {
    type Doc = nlp_serde.immutable.Document
    Json.fromJson[nlp_serde.immutable.Document](Json.parse(json))(docReads) match {
      case e: JsError => {
        println("Json: " + Json.prettyPrint(Json.parse(json)))
        println(e.errors.map(err => err._1 + ": " + err._2.mkString("\n")).mkString("\n\n"))
        System.exit(1)
        null
      }
      case r: JsSuccess[Doc] => r.get
    }
  }
}

