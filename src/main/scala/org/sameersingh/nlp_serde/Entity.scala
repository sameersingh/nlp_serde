package org.sameersingh.nlp_serde

import scala.collection.mutable
import org.sameersingh.nlp_serde.Util.Attr

/**
 * @author sameer
 * @since 9/1/14.
 */
class Entity extends Attr {
  var id: Int = _
  var representativeMId: Int = _
  var mids: mutable.Set[Int] = new mutable.HashSet
  var freebaseIds: mutable.Set[String] = new mutable.HashSet
  var ner: Option[String] = None
}
