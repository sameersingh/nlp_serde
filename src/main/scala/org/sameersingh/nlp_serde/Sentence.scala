package org.sameersingh.nlp_serde

import org.sameersingh.nlp_serde.Util.Attr
import scala.collection.mutable.ArrayBuffer

/**
 * @author sameer
 * @since 9/1/14.
 */
class Sentence extends Attr {
  var idx: Int = _
  var text: String = _
  var chars: (Int, Int) = _
  var parseTree: Option[String] = None
  var tokens: ArrayBuffer[Token] = new ArrayBuffer
  var mentions: ArrayBuffer[Mention] = new ArrayBuffer
  var relations: ArrayBuffer[Relation] = new ArrayBuffer
}
