package nlp_serde.annotators

import nlp_serde.{FileUtil, Document}
import org.sameersingh.nlp_serde.FileUtil
import scala.collection.mutable.{HashMap, HashSet}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import java.io.{InputStreamReader, BufferedReader, FileInputStream}
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
    println("Size: " + wikiToFreebase)
    print("Reading dictionary... ")
    val inputStream = new BZip2CompressorInputStream(new FileInputStream(dictionaryFile))
    val br = new BufferedReader(new InputStreamReader(inputStream))
    var l: String = ""
    while ({ l = br.readLine(); l} != null) {
      val split = l.split("\\t")
      if (split.length > 4) {
        val string = split(0)
        val prob = split(1).toDouble
        val wikiUrl = split(2)
        val fbId = wikiToFreebase(wikiUrl)
        if (prob > cutoffProb)
          dictionary(string) = dictionary.getOrElse(string, Seq.empty) ++ Seq(fbId -> prob)
      }
    }
    br.close()
    println("Done.")
    println("Size: " + dictionary.size)
    println("Barack Obama: " + dictionary("Barack Obama").mkString(", "))
  }

  def extractMid(url: String): String = url.replaceAll("<http://rdf.freebase.com/ns/", "").dropRight(1)

  def readWikiToFreebase: HashMap[String, String] = {
    print("Reading wiki to freebase... ")
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
    println("Done.")
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

object CrossWikiLinker {
  def main(args: Array[String]) {
    val dataHome = if (args.length > 0) args(0) else FileUtil.workDir + "/data/"
    val xwikiDict = dataHome + "crosswikis/orig/dictionary.bz2"
    val fb2Wiki = dataHome + "freebase/wikipedia.en.gz"

    val stanf = new StanfordAnnotator()
    val xwiki = new CrossWikiLinker(xwikiDict, fb2Wiki, 0.0)

    val d = new Document()
    d.id = "doc001"
    d.text = "Barack Obama is the president of the United States. He is married to Michelle Obama, and is not related to George Bush."

    stanf.process(d)
    xwiki.process(d)

    val pd = d.toCase
    println(pd.entities.mkString("\n"))
    println(pd.sentences.flatMap(_.mentions).mkString("\n"))
  }
}