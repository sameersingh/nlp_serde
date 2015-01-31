package nlp_serde.apps.d2d

import com.typesafe.scalalogging.slf4j.Logging
import nlp_serde.Document
import nlp_serde.readers.PerLineJsonReader

import scala.collection.mutable

/**
 * Created by sameer on 1/30/15.
 */
class IAITopicReader(dir: String = "data/d2d/iai/") extends Logging {

  // iaiID -> Topic
  val docTopics = new mutable.HashMap[String, Int]
  val topics = new mutable.HashMap[Int, Seq[String]]()
  val idToDate = new mutable.HashMap[String, String]

  def readTopics(model: String = "content"): Unit = {
    logger.info(s"Reading $model topics")
    val source = io.Source.fromFile(dir + model + "/temp_allafrica-topic-single.txt")
    for(l <- source.getLines()) {
      logger.trace(s"line: {$l}")
      val split = l.split("\t")
      assert(split.length == 2)
      logger.trace(s"split0: {${split(0)}}")
      logger.trace(s"split1: {${split(1)}}")
      val ids = split(0).split("\\/")
      val id = ids(0)
      if(id.isEmpty){
        logger.warn("Empty Id!: " + l)
      } else {
        val date = if (ids.length != 2) {
          logger.warn(s"Ids ${ids.mkString("{", ",", "}")} should be of length 2: ${split(0)}")
          ""
        } else ids(1)
        val topic = split(1).toInt
        docTopics(id) = topic
        idToDate(id) = date
        topics(topic) = topics.getOrElse(topic, Seq.empty) ++ Seq(id)
      }
    }
    logger.info(s" # topics: ${topics.size}")
    logger.info(s" # docs  : ${docTopics.size}")
  }

  val idToTitle = new mutable.HashMap[String, String]
  val urlToId = new mutable.HashMap[String, String]

  def readIAIIds(): Unit = {
    logger.info(s"Reading allafrica ids")
    val source = io.Source.fromFile(dir + "/allafrica_output.tsv", "ISO-8859-1")
    for(l <- source.getLines().drop(1)) {
      val split = l.split("\t").map(_.replaceAll("\"", ""))
      assert(split.length == 12)
      val id = split(0)
      val typ = split(1)
      val category = split(2)
      val title = split(3)
      val site = split(4)
      val url = split(5).replaceAll("http://allafrica.com/stories/", "").replaceAll(".html", "")
      val date = split(6)
      if(!url.isEmpty) {
        idToTitle(id) = title
        if(urlToId.contains(url))
          logger.warn(s"$url already exists with ${urlToId(url)}, can't add $id")
        urlToId(url) = id
      }
      assert(idToDate(id) == date)
    }
    logger.info(s" # docs  : ${idToTitle.size}")
  }

  def getTopic(d: Document): Option[Int] = {
    if(d.path.get.contains("allafrica.com")) {
      val url = d.path.get.replaceAll(".*stories/", "").replaceAll("\\.txt", "").take(12)
      val iaiID = urlToId.get(url)
      // println(d.path.get + "("+url+")")
      // println(iaiID)
      if(iaiID.isEmpty) logger.warn("cannot find id in " + d.path.get)
      val topic = docTopics.get(iaiID.get)
      if(topic.isEmpty) logger.warn("cannot find topic for " + iaiID.get)
      topic
    } else None
  }

}

object IAITopicReader {
  def main(args: Array[String]): Unit = {
    val input = args(0)
    val topic = args(1).toInt
    val topics = new IAITopicReader()
    topics.readTopics()
    topics.readIAIIds()
    for(doc <- new PerLineJsonReader().read(input)) {
      topics.getTopic(doc)
    }
  }
}
