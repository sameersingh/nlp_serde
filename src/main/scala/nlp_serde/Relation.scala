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

trait RelationType {
  // string that represents the complete relation type
  def string: String

  // human readable
  def pretty(arg1: String = "X", arg2: String = "Y"): String
}

trait Pattern extends RelationType {
  def name: String

  // string that represents the complete relation type
  override def string: String = "Pattern\tname"

  // human readable
  override def pretty(arg1: String = "X", arg2: String = "Y") = "%s %s %s" format(arg1, name, arg2)
}

trait FreebaseRelation extends RelationType {
  def mid: String

  def name: String

  // true if arg2 comes before arg1
  def reverse: Boolean

  // string that represents the complete relation type
  override def string: String = s"Freebase\t$name\t$mid\t$reverse"

  // human readable
  override def pretty(arg1: String = "X", arg2: String = "Y") = {
    val ord = if (reverse) (arg2, arg1) else (arg1, arg2)
    "%s is %s of %s" format(ord._1, name, ord._2)
  }
}
