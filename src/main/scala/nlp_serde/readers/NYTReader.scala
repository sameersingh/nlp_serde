package nlp_serde.readers

import java.io.File
import java.text.SimpleDateFormat

import com.nytlabs.corpus.{NYTCorpusDocument, NYTCorpusDocumentParser}
import nlp_serde.Document
import nlp_serde.writers.PerLineJsonWriter

/**
 * @author sameer
 * @since 12/23/14.
 */
class NYTReader(val filter: NYTCorpusDocument => Boolean = x => true, val validating: Boolean = true) extends DocPerFile {
  val parser = new NYTCorpusDocumentParser()

  override def readDoc(name: String): Option[Document] = {
    if(!name.endsWith(".txt")) return None
    val f = new File(name)
    val nytDoc = parser.parseNYTCorpusDocumentFromFile(f, validating)
    if(!filter(nytDoc)) return None
    val doc = new Document()
    doc.id = nytDoc.getGuid.toString
    println(nytDoc.toString)
    System.exit(1)
    doc.text
    val dateFormatter = new SimpleDateFormat("yyyy-MM-dd")
    doc.attrs("date") = dateFormatter.format(nytDoc.getPublicationDate)
    None
  }
}

object NYTReader {
  def main(args: Array[String]): Unit = {
    val nytBase = "/home/sameer/work/data/nyt/nyt/"
    val reader = new NYTReader()
    val docs = reader.readFilelist(nytBase + "docs/file.tbl", l => nytBase + "data" + l.drop(3))
    val writer = new PerLineJsonWriter(true)
    writer.write(nytBase + "../nyt.txt.json.gz", docs)
  }
}
