package org.sameersingh.nlp_serde

import scala.collection.mutable.ArrayBuffer
import org.sameersingh.nlp_serde.Util.Attr

/**
 * @author sameer
 * @since 9/1/14.
 */
class Document extends Attr {
  var id: String = _
  var text: String = _
  var path: Option[String] = None
  val sentences: ArrayBuffer[Sentence] = new ArrayBuffer
  val entities: ArrayBuffer[Entity] = new ArrayBuffer
}
