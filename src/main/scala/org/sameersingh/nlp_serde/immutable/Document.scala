package org.sameersingh.nlp_serde.immutable

/**
 * @author sameer
 * @since 9/2/14.
 */
case class Token(idx: Int, text: String, chars: (Int, Int),
                 lemma: Option[String], ner: Option[String], pos: Option[String],
                 attrs: Map[String, String])

case class Mention(id: Int, sentenceId: Int, posInSentence: Int, text: String,
                   toks: (Int, Int), headTokenIdx: Int,
                   mentionType: Option[String], ner: Option[String], entityId: Option[Int],
                   attrs: Map[String, String])

case class Relation(m1Id: Int, m2Id: Int, relations: Set[String],
                    attrs: Map[String, String])

case class Dep(label: String, source: Int, target: Int)

case class Sentence(idx: Int, text: String, chars: (Int, Int),
                    deps: Option[Seq[Dep]], parse: Option[String], tokens: Seq[Token],
                    mentions: Seq[Mention], relations: Seq[Relation],
                    attrs: Map[String, String])

case class Entity(id: Int, representativeMId: Int, representativeString: String,
                  mids: Set[Int], freebaseIds: Map[String, Double], ner: Option[String],
                  attrs: Map[String, String])

case class Document(id: String, text: String, path: Option[String],
                    sentences: Seq[Sentence], entities: Seq[Entity],
                    attrs: Map[String, String])