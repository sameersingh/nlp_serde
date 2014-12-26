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
  private var _id = 0

  def newDocId = {
    _id += 1
    "DOC%05d".format(_id - 1)
  }

  override def readDoc(name: String): Option[Document] = {
    val f = new File(name)
    if ((Set("NigeriaTwitterKeyWords",
      "China'sEconomyModerateGrowthTransformationandtheBrightFuture",
      "ConsulGeneralLIUKanInterviewedbyNewsAgencyofNigeria",
      "SpeechfromChineseConsulGeneralLiuKan", "BlogbyyusufJune2014", "README") contains f.getName.dropRight(4)) ||
      f.getCanonicalPath.contains("BokoHaramAbducts60WomenInAdamawa")) return None
    val lines = io.Source.fromFile(f).getLines().map(_.trim).filterNot(_.isEmpty).toSeq.view.force
    val doc = new Document()
    doc.path = Some(f.getCanonicalPath.replaceAll(".*/2015/demo-JIFX_15-2/", ""))
    doc.id = newDocId //doc.path.get
    val docType = D2DReader.extractType(f)
    doc.attrs("type") = docType.toString
    println(f.getName)
    try {
      val title = D2DReader.extractTitle(lines, docType)
      title.foreach(t => if (t.contains("Page has moved")) return None)
      title.foreach(t => doc.attrs("title") = t)
      val subtitle = D2DReader.extractSubTitle(lines, docType)
      subtitle.foreach(t => doc.attrs("subtitle") = t)
      val text = D2DReader.extractText(lines, docType)
      text.foreach(t => doc.text = t)
      val date = D2DReader.extractDate(lines, docType, f.getName)
      date.foreach(d => {
        val f = new SimpleDateFormat("yyyy-MM-dd")
        doc.attrs("date") = f.format(d)
      })
      if (!doc.attrs.contains("date")) {
        println(doc.toString)
        System.exit(1)
      }
    } catch {
      case e: Exception => {
        println("Error in reading: " + f.getCanonicalPath)
        e.printStackTrace()
        System.exit(1)
      }
    }
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
    val fromRandy, blog, aliBaba, news = Value
  }

  def extractType(f: File): DocType.Value = {
    val name = f.getName
    val parent = f.getParent
    if (name.startsWith("Blog")) return DocType.blog
    if (parent.endsWith("ali-baba")) return DocType.aliBaba
    if (parent.contains("from-randy")) return DocType.fromRandy
    else return DocType.news
    //println(s"name: $name, parent: $parent")
    assert(false, s"Everything should be covered by the above cases, but $name in $parent is not.")
    DocType.fromRandy
  }

  def extractTitle(lines: Seq[String], docType: DocType.Value): Option[String] = {
    if (docType == DocType.aliBaba)
      return Some(lines(0))
    if (docType == DocType.blog)
      return Some(lines(0))
    if (docType == DocType.fromRandy)
      return Some(lines(0))
    if (docType == DocType.news) {
      val idx = lines.indexWhere(_.contains("TITLE:"))
      assert(idx >= 0, "Could not find title in news story!")
      val titleString = lines(idx + 1)
      return Some(titleString)
    }
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
    if (docType == DocType.fromRandy) {
      if (lines(1).isEmpty || lines(1).charAt(0).isDigit) {
        return Some(lines(0) + ". " + lines.drop(2).mkString(" ").trim)
      }
      return Some(lines(0) + ". " + lines.drop(1).mkString(" ").trim)
    }
    if (docType == DocType.news) {
      val idx = lines.indexWhere(_.contains("TEXT:"))
      assert(idx >= 0, "Could not find text in news story!")
      return Some(lines.drop(idx + 1).mkString(" ").trim)
    }
    None
  }

  def extractDate(lines: Seq[String], docType: DocType.Value, fname: String): Option[Date] = {
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
    if (docType == DocType.fromRandy) {
      if (lines(1).contains("2014")) {
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
        if (lines(1).startsWith("BEIJING, ")) {
          val str = lines(1).replaceAll("BEIJING, ", "")
          val beijingDate = str.split(" ").take(3).mkString(" ")
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
      if (lines(2).contains("2014")) {
        val formatter = new SimpleDateFormat("dd MMMMM yyyy")
        return Some(formatter.parse(lines(2)))
      }
    }
    if (docType == DocType.news) {
      val idx = lines.indexWhere(_.contains("CITE:"))
      assert(idx >= 0, "Could not find date in news story!")
      try {
        val dateString = lines(idx + 1).split(" ").takeRight(3).mkString(" ")
        val formatter = new SimpleDateFormat("dd MMMMM yyyy")
        return Some(formatter.parse(dateString))
      } catch {
        case e: ParseException => {}
      }
      try {
        val dateString = lines(idx + 1).split(" ").drop(2).take(3).mkString(" ")
        val formatter = new SimpleDateFormat("dd MMMMM, yyyy")
        return Some(formatter.parse(dateString))
      } catch {
        case e: ParseException => {}
      }
      try {
        val dateString = lines(idx + 1).split(" ").drop(2).take(3).mkString(" ")
        val formatter = new SimpleDateFormat("MMMMM dd, yyyy")
        return Some(formatter.parse(dateString))
      } catch {
        case e: ParseException => {}
      }
      try {
        val dateString = lines(idx + 2).trim
        val formatter = new SimpleDateFormat("dd.MMM.yyyy")
        return Some(formatter.parse(dateString))
      } catch {
        case e: ParseException => {}
      }
      try {
        val dateString = lines(idx + 1).trim
        val formatter = new SimpleDateFormat("dd.MMM.yyyy")
        return Some(formatter.parse(dateString))
      } catch {
        case e: ParseException => {}
      }
      try {
        val dateString = lines(idx + 2).split(" ").takeRight(4).dropRight(1).mkString(" ")
        val formatter = new SimpleDateFormat("dd MMMMM yyyy")
        return Some(formatter.parse(dateString))
      } catch {
        case e: ParseException => {}
      }
      try {
        val dateString = fname.take(8)
        assert(dateString.forall(_.isDigit), "Could not find date in: %s (%s)".format(fname, lines.drop(idx).take(3).mkString("\n")))
        val formatter = new SimpleDateFormat("yyyyMMdd")
        return Some(formatter.parse(dateString))
      } catch {
        case e: ParseException => {
          println("Could not find date in: %s (%s)".format(fname, lines.drop(idx).take(3).mkString("\n")))
          System.exit(1)
        }
      }
    }
    None
  }

  def main(args: Array[String]): Unit = {
    val outputFile = "data/d2d/docs.txt.json.gz"
    val reader = new D2DReader
    val docs = reader.readFilelist("data/2015/demo-JIFX_15-2/filelist.names", "data/2015/demo-JIFX_15-2/" + _)
    //val annotator = new StanfordAnnotator()
    //val nlpDocs = annotator.process(docs)
    val writer = new PerLineJsonWriter(true)
    writer.write(outputFile, docs)
  }
}
