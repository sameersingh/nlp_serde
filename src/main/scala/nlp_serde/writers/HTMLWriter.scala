package nlp_serde.writers

import nlp_serde.{FileUtil, Document}

/**
 * Created by sameer on 11/7/14.
 */
class HTMLWriter(tokens: Boolean = false, deps: Boolean = false) extends DocPerFile {
  import org.sameersingh.htmlgen.ConverterUtils._
  import org.sameersingh.htmlgen.DivConverter.Implicits._

  override def writeDoc(name: String, doc: Document): Unit = {
    val writer = FileUtil.writer(name + "/" + FileUtil.name(doc.path.getOrElse(doc.id)) + ".html", false)
    val md = new Document(doc.toCase)
    if(!tokens) {
      md.sentences.foreach(s => s.tokens.clear())
    }
    if(!deps) {
      md.sentences.foreach(s => s.depTree = Some(Seq.empty))
    }
    val string = htmlWrap(md.toCase.asHTML.source)
    writer.println(string)
    writer.flush()
    writer.close()
  }
}
