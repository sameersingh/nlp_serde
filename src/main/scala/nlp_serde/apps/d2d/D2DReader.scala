package nlp_serde.apps.d2d

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import nlp_serde.annotators.StanfordAnnotator
import nlp_serde.writers.{PerLineJsonWriter, JsonWriter}
import nlp_serde.{FileFilters, Document}
import nlp_serde.readers.DocPerFile

import java.text.ParseException

/**
 * Created by sameer on 11/3/14.
 */
class D2DReader extends DocPerFile {
  override def readDoc(name: String): Option[Document] = {
    val f = new File(name)
    if (Set("NigeriaTwitterKeyWords",
      "China'sEconomyModerateGrowthTransformationandtheBrightFuture",
      "ConsulGeneralLIUKanInterviewedbyNewsAgencyofNigeria",
      "SpeechfromChineseConsulGeneralLiuKan") contains f.getName.dropRight(4)) return None
    val lines = io.Source.fromFile(f).getLines().map(_.trim).filterNot(_.isEmpty).toSeq.view.force
    val doc = new Document()
    doc.id = f.getName
    doc.path = Some(f.getCanonicalPath.replaceAll("/home/sameer/data/d2d/demo2015/nov/", ""))
    val docType = D2DReader.extractType(f)
    doc.attrs("type") = docType.toString
    val date = D2DReader.extractDate(lines, docType)
    date.foreach(d => {
      val f = new SimpleDateFormat("yyyy-MM-dd")
      doc.attrs("date") = f.format(d)
    })
    val title = D2DReader.extractTitle(lines, docType)
    title.foreach(t => doc.attrs("title") = t)
    val subtitle = D2DReader.extractSubTitle(lines, docType)
    subtitle.foreach(t => doc.attrs("subtitle") = t)
    val text = D2DReader.extractText(lines, docType)
    text.foreach(t => doc.text = t)
    if(!doc.attrs.contains("date")) println(doc.toString) else println(f.getName)
    Some(doc)
  }

  def readAll(baseDir: String) = {
    readDir(baseDir + "/ali-baba/", FileFilters.byExtension("txt")) ++
    readDir(baseDir + "/from-randy/", FileFilters.byExtension("txt")) ++
    readDir(baseDir + "/from-randy/blogs/", FileFilters.byExtension("txt"))
  }
}

object D2DReader {

  object DocType extends Enumeration {
    val news, blog, aliBaba = Value
  }

  def extractType(f: File): DocType.Value = {
    val name = f.getName
    val parent = f.getParent
    if (name.startsWith("Blog")) return DocType.blog
    if (parent.endsWith("ali-baba")) return DocType.aliBaba
    if (parent.endsWith("from-randy")) return DocType.news
    //println(s"name: $name, parent: $parent")
    assert(false, s"Everything should be covered by the above cases, but $name in $parent is not.")
    DocType.news
  }

  def extractTitle(lines: Seq[String], docType: DocType.Value): Option[String] = {
    if (docType == DocType.aliBaba)
      return Some(lines(0))
    if (docType == DocType.blog)
      return Some(lines(0))
    if (docType == DocType.news)
      return Some(lines(0))
    None
  }

  def extractSubTitle(lines: Seq[String], docType: DocType.Value): Option[String] = {
    if (docType == DocType.aliBaba) {
      val numHeader = 5 +
        (if (lines(2).startsWith("IIR") || lines(2).startsWith("TAC") || lines(2).startsWith("Gaza")) 0 else 1) +
        (if (lines(2).startsWith("Voice")) 1 else 0)
      val pruned = lines(2).split(" ").take(numHeader).mkString(" ").trim
      return Some(pruned)
    }
    // blog
    None
  }

  def extractText(lines: Seq[String], docType: DocType.Value): Option[String] = {
    if (docType == DocType.aliBaba) {
      val text = lines.drop(5).mkString("\n")
      val numHeader = 5 +
        (if (lines(2).startsWith("IIR") || lines(2).startsWith("TAC") || lines(2).startsWith("Gaza")) 0 else 1) +
        (if (lines(2).startsWith("Voice")) 1 else 0)
      val pruned = lines(2).split(" ").drop(numHeader).mkString(" ").trim
      return Some(pruned + " " + text.trim)
    }
    if (docType == DocType.blog) {
      if (lines(1).isEmpty || lines(1).charAt(0).isDigit) {
        return Some(lines.drop(2).mkString(" ").trim)
      }
      return Some(lines.drop(1).mkString(" ").trim)
    }
    if (docType == DocType.news) {
      if (lines(1).isEmpty || lines(1).charAt(0).isDigit) {
        return Some(lines(0) + ". " + lines.drop(2).mkString(" ").trim)
      }
      return Some(lines(0) + ". " + lines.drop(1).mkString(" ").trim)
    }
    None
  }

  def extractDate(lines: Seq[String], docType: DocType.Value): Option[Date] = {
    if (docType == DocType.aliBaba) {
      val formatter = new SimpleDateFormat("dd-MMM-yy")
      return Some(formatter.parse(lines(1)))
    }
    if (docType == DocType.blog) {
      if (!lines(1).isEmpty && lines(1).charAt(0).isDigit) {
        val formatter = new SimpleDateFormat("dd MMMMM yyyy")
        return Some(formatter.parse(lines(1)))
      }
      if (lines(0).startsWith("Blog by yusuf ") && lines(0).replaceAll("Blog by yusuf ", "").charAt(0).isDigit) {
        val formatter = new SimpleDateFormat("dd MMMMM yyyy")
        return Some(formatter.parse(lines(0).replaceAll("Blog by yusuf ", "")))
      }
      if (lines(0).startsWith("Blog 2 by yusuf ") && lines(0).replaceAll("Blog 2 by yusuf ", "").charAt(0).isDigit) {
        val formatter = new SimpleDateFormat("dd MMMMM yyyy")
        return Some(formatter.parse(lines(0).replaceAll("Blog 2 by yusuf ", "")))
      }
      if (lines(0).startsWith("Blog 3 by yusuf ") && lines(0).replaceAll("Blog 3 by yusuf ", "").charAt(0).isDigit) {
        val formatter = new SimpleDateFormat("dd MMMMM yyyy")
        return Some(formatter.parse(lines(0).replaceAll("Blog 3 by yusuf ", "")))
      }
      return None
    }
    if (docType == DocType.news) {
      if(lines(1).contains("2014")) {
        if (lines(1).charAt(0).isDigit) {
          val formatter = new SimpleDateFormat("dd MMMMM yyyy")
          return Some(formatter.parse(lines(1)))
        }
        // maybe MMMMM dd, yyyy or MMMMM dd yyyy
        try {
          val formatter = new SimpleDateFormat("MMMMM dd yyyy")
          return Some(formatter.parse(lines(1)))
        } catch {
          case e: ParseException => {}
        }
        try {
          val formatter = new SimpleDateFormat("MMMMM dd, yyyy")
          return Some(formatter.parse(lines(1)))
        } catch {
          case e: ParseException => {}
        }
        if(lines(1).startsWith("BEIJING, ")) {
          val str = lines(1).replaceAll("BEIJING, ", "")
          val beijingDate  = str.split(" ").take(3).mkString(" ")
          try {
            val formatter = new SimpleDateFormat("MMM. dd, yyyy")
            return Some(formatter.parse(beijingDate))
          } catch {
            case e: ParseException => {}
          }
          try {
            val formatter = new SimpleDateFormat("MMMMM dd, yyyy")
            return Some(formatter.parse(beijingDate))
          } catch {
            case e: ParseException => {}
          }
        }
      }
    }
    None
  }

  def main(args: Array[String]): Unit = {
    val outputFile = "/home/sameer/data/d2d/demo2015/nov/nigeria_dataset_v04.nlp.json.gz"
    val reader = new D2DReader
    val docs = reader.readAll("/home/sameer/data/d2d/demo2015/nov/nigeria_dataset_v04")
    val annotator = new StanfordAnnotator()
    val nlpDocs = annotator.process(docs)
    val writer = new PerLineJsonWriter(true)
    writer.write(outputFile, nlpDocs)
  }
}
