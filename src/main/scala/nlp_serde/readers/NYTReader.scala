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
  val classes = new collection.mutable.HashMap[String, Int]
  override def readDoc(name: String): Option[Document] = {
    if(!name.endsWith(".xml")) return None
    val f = new File(name)
    val nytDoc = parser.parseNYTCorpusDocumentFromFile(f, validating)
    if(!filter(nytDoc)) return None
    val doc = new Document()
    doc.id = nytDoc.getGuid.toString
    doc.path = Some(nytDoc.getSourceFile.getAbsolutePath.replaceAll(".*/data/", ""))
    //System.exit(1)
    doc.text = nytDoc.getBody
    val dateFormatter = new SimpleDateFormat("yyyy-MM-dd")
    doc.attrs("date") = dateFormatter.format(nytDoc.getPublicationDate)
    doc.attrs("taxonomicClassifiers") = nytDoc.getTaxonomicClassifiers.mkString("\t")
    doc.attrs("title") = nytDoc.getHeadline
    doc.attrs("onlineSection") = nytDoc.getOnlineSection
    doc.attrs("newsDesk") = nytDoc.getNewsDesk
    doc.attrs("typesOfMaterial") = nytDoc.getTypesOfMaterial.mkString("\t")
    doc.attrs("url") = nytDoc.getUrl.toString
    for(c <- nytDoc.getTaxonomicClassifiers) {
      classes(c) = 1 + classes.getOrElse(c, 0)
    }
    Some(doc)
  }
}

object NYTReader {
  def main(args: Array[String]): Unit = {
    val nytBase = "/home/sameer/work/data/nyt/nyt/"
    //val nytBase = "/Users/sameer/Work/data/nyt/nyt/"
    val reader = new NYTReader(_.getTaxonomicClassifiers.forall(!_.contains("Top/Classifieds")))
    val docs = reader.readFilelist(nytBase + "docs/files.tbl", l => nytBase + "data" + l.drop(3))
    val writer = new PerLineJsonWriter(true)
    writer.write(nytBase + "../nyt.txt.json.gz", docs)
    println(reader.classes.toSeq.sortBy(_._2).map(p => p._1 + "\t" + p._2).mkString("\n"))
  }
}
