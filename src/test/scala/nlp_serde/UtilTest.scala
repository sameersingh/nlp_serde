package nlp_serde

import nlp_serde.Util.Attr
import org.junit.Assert._
import org.junit._

class UtilTest {

  @Test
  def attrsTest() {
    class A(val a: Int) extends Attr
    val x = new A(10)
    assert(x.attrs.isEmpty)
    x.attrs("key") = "value1"
    assertEquals(x.attrs.size, 1)
    assertEquals(x.attrs("key"), "value1")
    x.attrs("key") = "value2"
    assertEquals(x.attrs.size, 1)
    assertEquals(x.attrs("key"), "value2")
    x.attrs("key2") = "value3"
    assertEquals(x.attrs.size, 2)
    assertEquals(x.attrs("key"), "value2")
    assertEquals(x.attrs("key2"), "value3")
  }

  @Test
  def mqlToWikiTitleTest() {
    val s1 = "\"Sameer$0020Singh\""
    println(FileUtil.extractWikiTitle(s1))
    val s2 = "\"Gabriel$0020Garc$00EDa$0020M$00E1rquez\""
    println(FileUtil.extractWikiTitle(s2))
    val s3 = "\"$002420$0020dollars\""
    println(FileUtil.extractWikiTitle(s3))
  }
}
