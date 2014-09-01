package org.sameersingh.nlp_serde

import scala.collection.mutable
import java.io.{FileFilter, File, FilenameFilter}


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

object FileFilters {
  val all = using(s => true)

  def byExtension(suffix: String) = using(s => s.endsWith(suffix))

  def startsWith(prefix: String) = using(s => s.startsWith(prefix))

  def using(f: String => Boolean) = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = f(name)
  }
}