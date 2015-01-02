package nlp_serde.annotators

import nlp_serde.Document

import scala.collection.JavaConversions._

/**
 * Named Entity Place Holder Annotator
 * creates a mention for each predicted named entity of certain types
 * and a corresponding entity for each mention.
 *
 * This annotator should run after StanfordAnnotator without dcoref.
 *
 * Created by xiaoling on 1/1/15.
 */
class NamedEntityMentionAnnotator extends Annotator {

  // only use entities of the following types
  lazy val validNerTypes = Set("PERSON", "ORGANIZATION", "LOCATION", "MISC")

  override def process(doc: Document): Document = {
    for (s <- doc.sentences) {
      val nerTokens = s.tokens.map(tok => tok.ner.get).zipWithIndex.filter(x => validNerTypes contains x._1)
      val startTokens = nerTokens.filter(x => !nerTokens.contains(x._1, x._2 - 1)).map(x => x._2)
      val endTokens = nerTokens.filter(x => !nerTokens.contains(x._1, x._2 + 1)).map(x => x._2 + 1)
      val spans = startTokens.zip(endTokens)
      val spanTags = nerTokens.filter(x => !nerTokens.contains(x._1, x._2 - 1)).map(x => x._1).zip(spans)
      for ((tag, span) <- spanTags) {
        val m = new nlp_serde.Mention()
        m.sentenceId = s.idx
        m.text = s.tokens.subList(m.toks._1 - 1, m.toks._2 - 1).map(_.text).mkString(" ")
        m.id = doc.entities.length + 1
        m.ner = Some(tag)
        m.mentionType = Some("PROPER")
        m.toks = (span._1 + 1, span._2 + 1)
        m.posInSentence
        s.mentions += m
        val e = new nlp_serde.Entity()
        e.id = m.id
        e.representativeMId = m.id
        e.representativeString = m.text
        e.ner = Some(m.ner.get)
        doc.entities += e
      }
    }
    doc
  }
}
