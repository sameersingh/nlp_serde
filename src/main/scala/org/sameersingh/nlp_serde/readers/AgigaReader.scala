package org.sameersingh.nlp_serde.readers

import org.sameersingh.nlp_serde._
import edu.jhu.agiga.{AgigaDocument, AgigaPrefs, StreamingDocumentReader}
import scala.collection.JavaConversions._
import scala.Some
import org.sameersingh.nlp_serde.immutable.Dep
import java.io.{StringWriter, Writer}

/**
 * @author sameer
 * @since 9/1/14.
 */
class AgigaReader extends DocsPerFile {

  private def writerToString(f: Writer => Unit): String = {
    val s = new StringWriter()
    f(s)
    s.toString
  }

  def convert(doc: AgigaDocument, fname: String): Document = {
    val d = new Document()
    d.id = doc.getDocId
    d.path = Some(fname)
    d.attrs("TYPE") = doc.getType
    d.attrs("PREFS_HASH") = doc.getPrefs.hashCode().toString
    //sentences
    for (sen <- doc.getSents) {
      val s = new Sentence()
      s.idx = sen.getSentIdx + 1
      s.text = writerToString(sen.writeWords)
      s.chars = sen.getTokens.head.getCharOffBegin -> sen.getTokens.last.getCharOffEnd
      s.depTree = Some(sen.getBasicDeps.map(td => Dep(td.getType, td.getGovIdx + 1, td.getDepIdx + 1))) // TODO: check
      s.parseTree = Some(sen.getParseText)

      for (tok <- sen.getTokens) {
        val t = new Token()
        t.idx = tok.getTokIdx + 1
        t.text = tok.getWord
        t.chars = (tok.getCharOffBegin - sen.getTokens.head.getCharOffBegin) -> (tok.getCharOffEnd - sen.getTokens.head.getCharOffBegin)
        t.lemma = Some(tok.getLemma)
        t.ner = Some(tok.getNerTag)
        t.pos = Some(tok.getPosTag)
        s.tokens += t
      }

      d.sentences += s
    }
    d.text = d.sentences.map(_.text).mkString("\n")
    //entities
    var mid = 1
    for (coref <- doc.getCorefs) {
      val e = new Entity()
      e.id = d.entities.size + 1
      for (men <- coref.getMentions) {
        val m = new Mention()
        m.id = mid
        mid += 1
        m.sentenceId = men.getSentenceIdx + 1
        val s = d.sentences(m.sentenceId - 1)
        s.mentions += m
        m.posInSentence = s.mentions.size
        m.toks = (men.getStartTokenIdx + 1) -> (men.getEndTokenIdx + 1)
        m.entityId = Some(e.id)
        m.text = (m.toks._1 - 1 until m.toks._2 - 1).map(i => s.tokens(i).text).mkString(" ")
        m.headTokenIdx = men.getHeadTokenIdx + 1
        val h = s.tokens(m.headTokenIdx - 1)
        m.ner = h.ner
        if (men.isRepresentative) {
          e.representativeMId = m.id
          e.representativeString = m.text
          e.ner = m.ner
        }
        e.mids += m.id
      }
      d.entities += e
    }
    d
  }

  override def read(name: String): Iterator[Document] = {
    assert(name.endsWith(".xml.gz"), "Filename should end with .xml.gz: %s" format (name))
    val prefs = new AgigaPrefs()
    prefs.setAll(true)
    new StreamingDocumentReader(name, prefs).iterator().map(d => convert(d, name))
  }
}

object AgigaReader {
  def main(args: Array[String]) {
    val dir = if (args.length > 0) args(0) else System.getProperty("user.home") + "/Work/data/agiga/data/";
    val reader = new AgigaReader
    var numDocs = 0
    for (d <- reader.readDir(dir, FileFilters.byExtension(".xml.gz"))) {
      if(numDocs == 0) println(d.toCase)
      numDocs += 1
    }
    println("Docs read: " + numDocs)
  }
}
