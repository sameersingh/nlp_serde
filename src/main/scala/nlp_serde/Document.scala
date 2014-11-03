package nlp_serde

import play.api.libs.json.Json

import scala.collection.mutable.ArrayBuffer

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

  def this(d: nlp_serde.immutable.Document) = {
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
