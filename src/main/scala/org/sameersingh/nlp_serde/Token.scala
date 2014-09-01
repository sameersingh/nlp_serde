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
}
