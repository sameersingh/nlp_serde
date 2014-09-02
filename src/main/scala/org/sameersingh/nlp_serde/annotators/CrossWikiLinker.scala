package org.sameersingh.nlp_serde.annotators

import org.sameersingh.nlp_serde.Document
import scala.collection.mutable.{HashMap, HashSet}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import java.io.FileInputStream
import java.util.zip.GZIPInputStream

/**
 * @author sameer
 * @since 9/2/14.
 */
class CrossWikiLinker(dictionaryFile: String, wikiUrlToFreebaseFile: String, cutoffProb: Double) extends Annotator {

  val dictionary: HashMap[String, Seq[(String, Double)]] = new HashMap

  readDictionary

  def readDictionary {
    val wikiToFreebase = readWikiToFreebase
    val inputStream = new BZip2CompressorInputStream(new FileInputStream(dictionaryFile))
    val source = io.Source.fromInputStream(inputStream)
    for (l <- source.getLines()) {
      val split = l.split("\\t")
      val string = split(0)
      val prob = split(1).toDouble
      val wikiUrl = split(2)
      val fbId = wikiToFreebase(wikiUrl)
      if (prob > cutoffProb)
        dictionary(string) = dictionary.getOrElse(string, Seq.empty) ++ Seq(fbId -> prob)
    }
    source.close()
  }

  def readWikiToFreebase: HashMap[String, String] = {
    val wikiToFreebase: HashMap[String, String] = new HashMap
    val inputStream = new GZIPInputStream(new FileInputStream(wikiUrlToFreebaseFile))
    val source = io.Source.fromInputStream(inputStream)
    for (l <- source.getLines()) {
      val split = l.split("\\t")
      val wikiUrl = split(0)
      val fbId = split(1)
      wikiToFreebase(wikiUrl) = fbId
    }
    source.close()
    wikiToFreebase
  }

  override def process(doc: Document): Document = {
    for (e <- doc.entities) {
      val string = e.representativeString
      val fbIds = dictionary.getOrElse(string, Seq.empty)
      e.freebaseIds ++= fbIds.sortBy(-_._2)
    }
    doc
  }
}
