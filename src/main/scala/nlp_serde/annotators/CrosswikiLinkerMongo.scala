package nlp_serde.annotators

import java.text.Normalizer

import com.mongodb.{BasicDBObject, MongoClient, DB, DBCollection}
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.PerLineJsonWriter
import nlp_serde.{Mention, Document}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by xiaoling on 11/6/14.
 */
class CrosswikiLinkerMongo(dbHost: String, dbPort: Int = 27017, dbName: String, numCandidates: Int = 30) extends Annotator {
  val mongoClient = new MongoClient(dbHost, dbPort)
  val db = mongoClient.getDB(dbName)
  val wpidColl = db.getCollection("wpid2name")
  val nameColl = db.getCollection("name2wpid")
  val xwikisColl = db.getCollection("crosswikis")

  def isDisambiguation(wikiTitle: String) = wikiTitle.contains("(disambiguation)")

  def close() = try {
    if (mongoClient != null) {
      mongoClient.close()
    }
  } catch {
    case e: Exception => e.printStackTrace()
  }

  def getLnrm(string: String): String = {
    var lnrm = Normalizer.normalize(string, Normalizer.Form.NFD)
    lnrm = lnrm.replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    lnrm = lnrm.toLowerCase()
    lnrm = lnrm.replaceAll("[^\\p{Alnum}]+", "")
    lnrm
  }

  def getCandidates(mention: String): Seq[(String, Double)] = {
    val candidates = new ArrayBuffer[(String, Double)]
    val lrnm = getLnrm(mention)
//    println("lrnm=" + lrnm)
    val cursor = xwikisColl.find(new BasicDBObject("anchor", lrnm))
      while (cursor.hasNext) {
        val cand = cursor.next()
        val mid = getFreebaseIdFromWpid(getWpidFromTitle(cand.get("entity").asInstanceOf[String]))
        if (mid != null) {
          val score = cand.get("cprob").asInstanceOf[Double]
          candidates += ((mid, score))
        }
      }
    candidates.sortBy(-_._2).toSeq.take(numCandidates)
  }

  def getWpidFromTitle(title: String): Int = {
    val result = nameColl.findOne(new BasicDBObject("name", title.replace("_", " ")))
    if (result == null) {
      -1
    } else {
      result.get("wpid").asInstanceOf[Int]
    }
  }

  def getFreebaseIdFromWpid(wpid: Int): String = {
    if (wpid < 0) {
      null
    } else {
      val result = wpidColl.findOne(new BasicDBObject("wpid", wpid))
      if (result == null) {
        null
      } else {
        result.get("mid").asInstanceOf[String]
      }
    }
  }

  override def process(doc: Document): Document = {
    print("linking doc="+doc.id)
    for (e <- doc.entities) {
      print(".")
      // find the best name for linking
//      println(e.toCase.toString)
//      println("rep = "+e.representativeString+","+e.representativeMId)

      var string = e.representativeString
      var repMention: Mention = null
      val sent = doc.sentences.find(s => s.mentions.exists(m => if (m.id == e.representativeMId) {
        repMention = m
        string = repMention.text
//        println("string="+string)
        true
      } else false))
//      println("sent="+sent.get.toCase.toString)
      sent match {
        case Some(s) => for (m <- s.mentions) {
          if (repMention != m && m.toks._1 >= repMention.toks._1 && m.toks._2 <= repMention.toks._2 && m.mentionType.getOrElse("").equals("PROPER")) {
            // found a better mention for linking
            println("replace the string (" + string + ") by " + m.text)
            string = m.text
          }

        }
        case None => println("repMention not found in any sentence, mention=" + e.representativeMId + ", entity=" + e.id + ", doc=" + doc.id)
      }
//      println("candidates for "+string)
      //      val fbIds = dictionary.getOrElse(string, Seq.empty)
      val fbIds = getCandidates(string)
      e.freebaseIds ++= fbIds
    }
    println()
    doc
  }
}

object TestCrosswikisLinker {
  def main(args: Array[String]): Unit = {
    val linker = new CrosswikiLinkerMongo(dbHost = "rv-n12", dbName = "vinculum")
    val wpid = linker.getWpidFromTitle("Barack Obama")
    println(wpid)
    val mid = linker.getFreebaseIdFromWpid(wpid)
    println(mid)
    val candidates = linker.getCandidates("Washington")
    candidates.foreach(println)
    linker.close()
  }
}

object RunCrosswikisLinkerForD2D {
  def main(args: Array[String]): Unit = {
    val linker = new CrosswikiLinkerMongo(dbHost = "rv-n12", dbName = "vinculum")
    val reader = new PerLineJsonReader(true)
    val docs = reader.read("nigeria_dataset_v04.nlp.json.gz")
    val nlpDocs = linker.process(docs)
    val writer = new PerLineJsonWriter(true)
    writer.write("nigeria_dataset_v04.nlp.cw.json.gz", nlpDocs)
  }
}