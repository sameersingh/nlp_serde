package nlp_serde

import nlp_serde.Util.Attr
import nlp_serde.immutable.Dep

import scala.collection.mutable.ArrayBuffer

/**
 * @author sameer
 * @since 9/1/14.
 */
class Sentence extends Attr {
  // 1-indexed
  var idx: Int = _
  var text: String = _
  var chars: (Int, Int) = _
  var depTree: Option[Seq[Dep]] = None
  var parseTree: Option[String] = None
  var tokens: ArrayBuffer[Token] = new ArrayBuffer
  var mentions: ArrayBuffer[Mention] = new ArrayBuffer
  var relations: ArrayBuffer[Relation] = new ArrayBuffer

  def this(s: nlp_serde.immutable.Sentence) {
    this()
    idx = s.idx
    text = s.text
    chars = s.chars
    depTree = s.deps
    parseTree = s.parse
    tokens ++= s.tokens.map(t => new Token(t))
    mentions ++= s.mentions.map(t => new Mention(t))
    relations ++= s.relations.map(t => new Relation(t))
    attrs ++= s.attrs
  }

  def toCase = immutable.Sentence(idx, text, chars, depTree, parseTree,
    tokens.map(_.toCase), mentions.map(_.toCase), relations.map(_.toCase), attrs.toMap)
}
