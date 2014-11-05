package nlp_serde

import org.junit.Test
import play.api.libs.json.Json

/**
 * @author sameer
 * @since 9/1/14.
 */
class JsonTest {
  def sampleDoc = {
    val d = new Document
    d.id = "doc001"
    d.text = "This is a document that contains 2 sentences. One is the previous one, and the other is this one."
    d
  }

  def sampleJsonDoc =
    """
      |{
      |  "id": "doc001",
      |  "text": "This is a document that contains 2 sentences. One is the previous one, and the other is this one.",
      |  "sentences":[],
      |  "entities":[],
      |  "attrs":{}
      |}
    """.stripMargin

  @Test
  def testJsonWrite() {
    val d = sampleDoc.toCase
    println(Json.prettyPrint(JsonUtil.fromDoc(d)))
  }

  @Test
  def testJsonRead() {
    val json = sampleJsonDoc
    val d = JsonUtil.toDoc(json)
    println(d)
  }
}
