package nlp_serde

import nlp_serde.Util.Attr

import scala.collection.mutable

/**
 * @author sameer
 * @since 9/1/14.
 */
class Entity extends Attr {
  // 1-indexed
  var id: Int = _
  // 1-indexed
  var representativeMId: Int = _
  var representativeString: String = _
  var mids: mutable.Set[Int] = new mutable.HashSet
  var freebaseIds: mutable.Map[String, Double] = new mutable.HashMap
  var ner: Option[String] = None

  def this(d: nlp_serde.immutable.Entity) = {
    this()
    id = d.id
    representativeMId = d.representativeMId
    representativeString = d.representativeString
    mids ++= d.mids
    freebaseIds ++= d.freebaseIds
    ner = d.ner
    attrs ++= d.attrs
  }

  def toCase = immutable.Entity(id, representativeMId, representativeString, mids.toSet, freebaseIds.toMap, ner, attrs.toMap)
}
