package org.sameersingh.nlp_serde

import org.sameersingh.nlp_serde.Util.Attr

/**
 * @author sameer
 * @since 9/1/14.
 */
class Token extends Attr {
  var idx: Int = _
  var text: String = _
  var chars: (Int, Int) = _
  var stem: Option[String] = None
  var ner: Option[String] = None
  var pos: Option[String] = None

  def this(t: immutable.Token) = {
    this()
    idx = t.idx
    text = t.text
    chars = t.chars
    stem = t.stem
    ner = t.ner
    pos = t.pos
    attrs ++= t.attrs
  }

  def toCase = immutable.Token(idx, text, chars, stem, ner, pos, attrs.toMap)
}
