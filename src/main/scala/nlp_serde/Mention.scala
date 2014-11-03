package nlp_serde

/**
 * @author sameer
 * @since 9/1/14.
 */
class Mention extends Attr {
  // 1-indexed
  var id: Int = _
  // 1-indexed
  var sentenceId: Int = _
  // 1-indexed
  var posInSentence: Int = _
  // 1-indexed
  var toks: (Int, Int) = _
  // 1-indexed
  var headTokenIdx: Int = _

  var text: String = _
  var mentionType: Option[String] = None
  var ner: Option[String] = None
  // 1-indexed
  var entityId: Option[Int] = None

  def this(m: nlp_serde.immutable.Mention) {
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

  def toCase = immutable.Mention(id, sentenceId, posInSentence, text, toks, headTokenIdx, mentionType, ner, entityId, attrs.toMap)
}
