package org.sameersingh.nlp_serde

import org.sameersingh.nlp_serde.Util.Attr

/**
 * @author sameer
 * @since 9/1/14.
 */
class Mention extends Attr {
  var id: Int = _
  var sentenceId: Int = _
  var text: String = _
  var toks: (Int, Int) = _
  var headTokenIdx: Option[Int] = None
  var mentionType: Option[String] = None
  var ner: Option[String] = None
  var entityId: Option[String] = None

  def this(m: immutable.Mention) {
    this()
    id = m.id
    sentenceId = m.sentenceId
    text = m.text
    toks = m.toks
    headTokenIdx = m.headTokenIdx
    mentionType = m.mentionType
    ner = m.ner
    entityId = m.entityId
    attrs ++= m.attrs
  }

  def toCase = immutable.Mention(id, sentenceId, text, toks, headTokenIdx, mentionType, ner, entityId, attrs.toMap)
}
