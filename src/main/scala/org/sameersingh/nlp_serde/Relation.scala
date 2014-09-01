package org.sameersingh.nlp_serde

import org.sameersingh.nlp_serde.Util.Attr
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable

/**
 * @author sameer
 * @since 9/1/14.
 */
class Relation extends Attr {
  var m1Id: Int = _
  var m2Id: Int = _
  var relations: mutable.Set[String] = new mutable.HashSet
}
