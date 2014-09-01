package org.sameersingh.nlp_serde

import scala.collection.mutable


/**
 * @author sameer
 * @since Aug 31 2014
 */
object Util {

  trait Attr {
    val _attrs: mutable.Map[String, String] = new mutable.HashMap[String, String]

    def attrs = _attrs
  }

}
