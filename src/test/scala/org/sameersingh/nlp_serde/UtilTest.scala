package org.sameersingh.nlp_serde

import org.junit._
import Assert._
import org.sameersingh.nlp_serde.Util.Attr

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
}
