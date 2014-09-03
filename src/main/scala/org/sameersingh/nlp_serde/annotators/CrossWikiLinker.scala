package org.sameersingh.nlp_serde.annotators

import org.sameersingh.nlp_serde.{FileUtil, Document}
import scala.collection.mutable.{HashMap, HashSet}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import java.io.FileInputStream
import java.util.zip.GZIPInputStream
import java.util.regex.Pattern

/**
 * A simple cross wiki based linker
 *
 * @param dictionaryFile dictionary.bz2 distribtued with the Cross-Wiki dataset
 * @param wikiUrlToFreebaseFile gzipped file created by grepping freebase dump with "wikipedia.en"
 * @author sameer
 * @since 9/2/14.
 */
class CrossWikiLinker(dictionaryFile: String, wikiUrlToFreebaseFile: String, cutoffProb: Double) extends Annotator {

  val dictionary: HashMap[String, Seq[(String, Double)]] = new HashMap

  readDictionary

  def readDictionary {
    // e.g.: "A Piece of the Action" 0.777778 A_Piece_of_the_Action_(Star_Trek) NR RWB W:6/7 W08 W09 WDB w':1/2
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

  def extractMid(url: String): String = url.replaceAll("<http://rdf.freebase.com/ns/", "").dropRight(1)

  def readWikiToFreebase: HashMap[String, String] = {
    // e.g.: <http://rdf.freebase.com/ns/m.01009ly3> <http://rdf.freebase.com/key/wikipedia.en>      "Dysdera_ancora"        .
    val wikiToFreebase: HashMap[String, String] = new HashMap
    val inputStream = new GZIPInputStream(new FileInputStream(wikiUrlToFreebaseFile))
    val source = io.Source.fromInputStream(inputStream)
    for (l <- source.getLines()) {
      val split = l.split("\\t")
      val mid = extractMid(split(0))
      val wikiTitle = FileUtil.extractWikiTitle(split(2))
      wikiToFreebase(wikiTitle) = mid
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
