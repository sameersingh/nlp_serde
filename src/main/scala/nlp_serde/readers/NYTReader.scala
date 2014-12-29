package nlp_serde.readers

import java.io.File
import java.text.SimpleDateFormat

import com.nytlabs.corpus.{NYTCorpusDocument, NYTCorpusDocumentParser}
import nlp_serde.Document
import nlp_serde.writers.PerLineJsonWriter
import scala.collection.JavaConversions._

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
