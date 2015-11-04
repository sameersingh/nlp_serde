package nlp_serde.readers

import java.io.{FilenameFilter, File}
import java.text.SimpleDateFormat

import com.nytlabs.corpus.{NYTCorpusDocument, NYTCorpusDocumentParser}
import nlp_serde.Document
import nlp_serde.writers.PerLineJsonWriter
import nlp_serde.annotators.StanfordAnnotator
import nlp_serde.FileUtil
import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * @author sameer
 * @since 12/23/14.
 */
class NYTReader(val filter: NYTCorpusDocument => Boolean = x => true, val validating: Boolean = false) extends DocPerFile {
  val parser = new NYTCorpusDocumentParser()
  val taxonomicClassifiers = new collection.mutable.HashMap[String, Int]
  val typesofMaterials = new collection.mutable.HashMap[String, Int]

  override def readDoc(name: String): Option[Document] = {
    if (!name.endsWith(".xml")) return None
    val f = new File(name)
    val nytDoc = parser.parseNYTCorpusDocumentFromFile(f, validating)
    if (!filter(nytDoc)) return None
    if (nytDoc.getBody == null) return None
    val doc = new Document()
    doc.id = nytDoc.getGuid.toString
    doc.path = Some(nytDoc.getSourceFile.getAbsolutePath.replaceAll(".*/data/", ""))
    doc.text = nytDoc.getBody
    val dateFormatter = new SimpleDateFormat("yyyy-MM-dd")
    Option(nytDoc.getPublicationDate).foreach(v => doc.attrs("date") = dateFormatter.format(v))
    Option(nytDoc.getTaxonomicClassifiers).foreach(v => doc.attrs("taxonomicClassifiers") = v.mkString("\t"))
    Option(nytDoc.getHeadline).foreach(v => doc.attrs("title") = v)
    Option(nytDoc.getOnlineSection).foreach(v => doc.attrs("onlineSection") = v)
    Option(nytDoc.getNewsDesk).foreach(v => doc.attrs("newsDesk") = v)
    Option(nytDoc.getTypesOfMaterial).foreach(v => doc.attrs("typesOfMaterial") = v.mkString("\t"))
    Option(nytDoc.getUrl).foreach(v => doc.attrs("url") = v.toString)
    for (c <- nytDoc.getTaxonomicClassifiers) {
      taxonomicClassifiers(c) = 1 + taxonomicClassifiers.getOrElse(c, 0)
    }
    for (t <- nytDoc.getTypesOfMaterial) {
      typesofMaterials(t) = 1 + typesofMaterials.getOrElse(t, 0)
    }
    Some(doc)
  }
}

object NYTReader {

  def filterNonArticles(d: NYTCorpusDocument): Boolean = {
    d.getTaxonomicClassifiers.forall(!_.contains("Top/Classifieds")) &&
      d.getTypesOfMaterial.forall(_.toLowerCase != "list") &&
      d.getTypesOfMaterial.forall(!_.toLowerCase.contains("statistic"))
  }

  def main(args: Array[String]): Unit = {
    val nytBase = "data/nyt/nyt/"
    //val nytBase = "/Users/sameer/Work/data/nyt/nyt/"
    val reader = new NYTReader(filterNonArticles)
    val docs = reader.readFilelist(nytBase + "docs/file.tbl", l => nytBase + "data" + l.drop(3))
    val writer = new PerLineJsonWriter(true)
    writer.write(nytBase + "../nyt.txt.json.gz", docs)
    println(reader.typesofMaterials.toSeq.sortBy(_._2).map(p => p._1 + "\t" + p._2).mkString("\n"))
  }
}

object NYTFileListAnnotator {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Not enough arguments: filelist nyt_dir")
      System.exit(1)
    }
    val input = args(0)
    val output = args(0) + ".nlp.json.gz"
    val nytDir = args(1)

    val annotator = new StanfordAnnotator(Seq("tokenize", "ssplit", "pos", "lemma", "ner", "parse"), false)
    val s = FileUtil.inputSource(input, false)
    val nytReader = new NYTReader()
    val docs = s.getLines().map(_.replaceAll("_", "/")).flatMap(l => nytReader.readDoc(nytDir + "/" + l)).map(d => annotator.process(d))

    val w = new PerLineJsonWriter(true)
    w.write(output, docs)

    s.close()
  }
}

object VerbCounts {
  def writeMap(fname: String, map: Iterable[(String, Long)]): Unit = {
    val writer = FileUtil.writer(fname, true)
    for((v,c) <- map) {
      writer.println(c + "\t" + v)
    }
    writer.flush()
    writer.close()
  }
  def main(args: Array[String]): Unit = {
    val reader = new PerLineJsonReader(true)

    val docs = reader.readDir("data/nyt", new FilenameFilter {
      override def accept(file: File, s: String): Boolean = s.endsWith(".nlp.json.gz")
    })

    val counts = new mutable.HashMap[String, Long]
    val newsDesk = new mutable.HashMap[String, Long]
    val taxonomicClassifiers = new mutable.HashMap[String, Long]
    val onlineSection = new mutable.HashMap[String, Long]
    var idx = 0l
    for(d <- docs) {
      d.attrs.get("taxonomicClassifiers").foreach(s => s.split("\t").foreach(t => {
        taxonomicClassifiers(t) = 1 + taxonomicClassifiers.getOrElse(t, 0l)
      }))
      d.attrs.get("newsDesk").foreach(t => newsDesk(t) = 1 + newsDesk.getOrElse(t, 0l))
      d.attrs.get("onlineSection").foreach(t => onlineSection(t) = 1 + onlineSection.getOrElse(t, 0l))
      if(d.attrs.get("newsDesk").map(d => d == "National Desk" || d == "Foreign Desk").getOrElse(false)) {
        for (s <- d.sentences; t <- s.tokens) {
          if (t.pos.get.startsWith("V")) {
            counts(t.lemma.get) = 1 + counts.getOrElse(t.lemma.get, 0l)
            idx += 1
            if (idx % 100000 == 0) {
              println("counts: " + counts.size + "\t" + counts.toSeq.sortBy(-_._2).take(5).mkString(", "))
            }
            if (idx % 1000000l == 0) {
              println("Writing files...")
              writeMap("data/nyt/verb_counts.gz", counts)
              writeMap("data/nyt/newsDesks.gz", newsDesk)
              writeMap("data/nyt/taxonomicClassifiers.gz", taxonomicClassifiers)
              writeMap("data/nyt/onlineSections.gz", onlineSection)
            }
          }
        }
      }
    }
    writeMap("data/nyt/verb_counts.gz", counts)
    writeMap("data/nyt/newsDesks.gz", newsDesk)
    writeMap("data/nyt/taxonomicClassifiers.gz", taxonomicClassifiers)
    writeMap("data/nyt/onlineSections.gz", onlineSection)
  }
}