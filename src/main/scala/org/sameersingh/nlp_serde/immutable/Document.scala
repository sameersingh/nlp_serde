package org.sameersingh.nlp_serde.immutable

import org.sameersingh.nlp_serde.Util

/**
 * @author sameer
 * @since 9/2/14.
 */
case class Token(idx: Int, text: String, chars: (Int, Int),
                 lemma: Option[String], ner: Option[String], pos: Option[String],
                 attrs: Map[String, String]) {
  override def toString: String = toString(0)

  def toString(indent: Int): String = {
    Util.indent(indent, "%d\t%s\t%s\t%s\t%s\t%s\t%s".format(idx, text, chars,
      lemma.getOrElse("-"), ner.getOrElse("-"), pos.getOrElse("-"), attrs.map({
        case (k, v) => k + ": " + v
      }).mkString("\t")))
  }
}

case class Mention(id: Int, sentenceId: Int, posInSentence: Int, text: String,
                   toks: (Int, Int), headTokenIdx: Int,
                   mentionType: Option[String], ner: Option[String], entityId: Option[Int],
                   attrs: Map[String, String]) {
  override def toString: String = toString(0)

  def toString(indent: Int): String = {
    val sb = new StringBuilder
    sb ++= Util.indent(indent, "Mention: %d, sen: %d, posInSen: %d%s\n" format(id, sentenceId, posInSentence, entityId.map(e => ", ent: " + e).getOrElse("")))
    sb ++= Util.indent(indent + 1, text.trim + " " + toks + ":" + headTokenIdx + "\n")
    mentionType.foreach(m => sb.append(Util.indent(indent + 1, "Type: " + m) + "\n"))
    attrs.foreach({
      case (k, v) => sb ++= Util.indent(indent + 1, k + ": " + v + "\n")
    })
    sb.toString()
  }
}

case class Relation(m1Id: Int, m2Id: Int, relations: Set[String],
                    attrs: Map[String, String]) {
  def toString(indent: Int): String = Util.indent(indent, toString())
}

case class Dep(label: String, source: Int, target: Int) {
  override def toString: String = target + "-%s->".format(label) + source
}

case class Sentence(idx: Int, text: String, chars: (Int, Int),
                    deps: Option[Seq[Dep]], parse: Option[String], tokens: Seq[Token],
                    mentions: Seq[Mention], relations: Seq[Relation],
                    attrs: Map[String, String]) {
  override def toString: String = toString(0)

  def toString(indent: Int): String = {
    val sb = new StringBuilder
    sb ++= Util.indent(indent, "Sentence: " + idx + " " + chars + "\n")
    sb ++= Util.indent(indent + 1, text.trim + "\n")
    deps.foreach(d => sb ++= Util.indent(indent + 1, "Dep: " + d.mkString(", ") + "\n"))
    parse.foreach(p => sb ++= Util.indent(indent + 1, "Parse: " + p + "\n"))
    tokens.foreach(t => sb ++= t.toString(indent + 1) + "\n")
    mentions.foreach(m => sb ++= m.toString(indent + 1) + "\n")
    relations.foreach(r => sb ++= r.toString(indent + 1) + "\n")
    attrs.foreach({
      case (k, v) => sb ++= Util.indent(indent + 1, k + ": " + v + "\n")
    })
    sb.toString()
  }
}

case class Entity(id: Int, representativeMId: Int, representativeString: String,
                  mids: Set[Int], freebaseIds: Map[String, Double], ner: Option[String],
                  attrs: Map[String, String]) {
  override def toString: String = toString(0)

  def toString(indent: Int): String = {
    val sb = new StringBuilder
    sb ++= Util.indent(indent, "Entity: %d, %s (%d)\n" format(id, representativeString, representativeMId))
    sb ++= Util.indent(indent+1, "Mentions: " + mids.mkString(", ") + "\n")
    if (!freebaseIds.isEmpty)
      sb ++= Util.indent(indent+1, "Freebase: " + freebaseIds.mkString(", ") + "\n")
    ner.foreach(n => sb ++= Util.indent(indent+1, "Type: " + n + "\n"))
    attrs.foreach({
      case (k, v) => sb ++= Util.indent(indent + 1, k + ": " + v + "\n")
    })
    sb.toString()
  }
}

case class Document(id: String, text: String, path: Option[String],
                    sentences: Seq[Sentence], entities: Seq[Entity],
                    attrs: Map[String, String]) {

  override def toString: String = toString(0)

  def toString(indent: Int): String = {
    val sb = new StringBuilder
    sb ++= Util.indent(indent, "Document: " + id + "\n")
    path.foreach(p => sb ++= Util.indent(indent + 1, "Path: " + p) + "\n")
    sb ++= Util.indent(indent + 1, text + "\n")
    sentences.foreach(s => sb ++= s.toString(indent + 1) + "\n")
    entities.foreach(s => sb ++= s.toString(indent + 1) + "\n")
    attrs.foreach({
      case (k, v) => sb ++= Util.indent(indent + 1, k + ": " + v + "\n")
    })
    sb.toString()
  }
}