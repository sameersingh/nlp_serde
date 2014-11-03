package nlp_serde

/**
 * @author sameer
 * @since 9/1/14.
 */
class Token extends Attr {
  // 1-indexed
  var idx: Int = _
  var text: String = _
  var chars: (Int, Int) = _
  var lemma: Option[String] = None
  var ner: Option[String] = None
  var pos: Option[String] = None

  def this(t: nlp_serde.immutable.Token) = {
    this()
    idx = t.idx
    text = t.text
    chars = t.chars
    lemma = t.lemma
    ner = t.ner
    pos = t.pos
    attrs ++= t.attrs
  }

  def toCase = immutable.Token(idx, text, chars, lemma, ner, pos, attrs.toMap)
}
