package nlp_serde

import nlp_serde.Util.Attr

import scala.collection.mutable

/**
 * @author sameer
 * @since 9/1/14.
 */
class Relation extends Attr {
  var m1Id: Int = _
  var m2Id: Int = _
  var relations: mutable.Set[String] = new mutable.HashSet

  def this(r: nlp_serde.immutable.Relation) {
    this()
    m1Id = r.m1Id
    m2Id = r.m2Id
    relations ++= r.relations
    attrs ++= r.attrs
  }

  def toCase = immutable.Relation(m1Id, m2Id, relations.toSet, attrs.toMap)
}
