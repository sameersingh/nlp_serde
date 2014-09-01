package org.sameersingh.nlp_serde

import scala.collection.mutable.ArrayBuffer
import org.sameersingh.nlp_serde.Util.Attr
import play.api.libs.json.Json

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

  def this(d: immutable.Document) = {
    this()
    id = d.id
    text = d.text
    path = d.path
    sentences ++= d.sentences.map(s => new Sentence(s))
    entities ++= d.entities.map(e => new Entity(e))
  }

  override def toString: String = Json.prettyPrint(JsonUtil.fromDoc(toCase))

  def toCase = immutable.Document(id, text, path, sentences.map(_.toCase), entities.map(_.toCase), attrs.toMap)
}

object immutable {

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
                      deps: Option[Seq[Dep]], parse: Option[String], tokens: Seq[immutable.Token],
                      mentions: Seq[immutable.Mention], relations: Seq[immutable.Relation],
                      attrs: Map[String, String])

  case class Entity(id: Int, representativeMId: Int, representativeString: String,
                    mids: Set[Int], freebaseIds: Set[String], ner: Option[String],
                    attrs: Map[String, String])

  case class Document(id: String, text: String, path: Option[String],
                      sentences: Seq[immutable.Sentence], entities: Seq[immutable.Entity],
                      attrs: Map[String, String])

}