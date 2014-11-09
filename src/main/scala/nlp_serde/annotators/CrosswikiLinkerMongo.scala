package nlp_serde.annotators

import java.text.Normalizer

import com.mongodb.{BasicDBObject, MongoClient, DB, DBCollection}
import edu.washington.cs.figer.analysis.MapType
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.PerLineJsonWriter
import nlp_serde.{Sentence, Mention, Document}

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

/**
 * Created by xiaoling on 11/6/14.
 */
class CrosswikiLinkerMongo(dbHost: String, dbPort: Int = 27017, dbName: String, numCandidates: Int = 30, useFiger: Boolean = false) extends Annotator {
  val mongoClient = new MongoClient(dbHost, dbPort)
  val db = mongoClient.getDB(dbName)
  val wpidColl = db.getCollection("wpid2name")
  val nameColl = db.getCollection("name2wpid")
  val xwikisColl = db.getCollection("crosswikis")

  if (useFiger) {
    MapType.init
  }


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

  def getCandidates(mention: Mention): Map[String, Double] = {
    val candidates = new ArrayBuffer[(Int, Double)]
    val lrnm = getLnrm(mention.text)
    //    println("lrnm=" + lrnm)
    val cursor = xwikisColl.find(new BasicDBObject("anchor", lrnm))
    while (cursor.hasNext) {
      val cand = cursor.next()
      val wpid = getWpidFromTitle(cand.get("entity").asInstanceOf[String])
      if (wpid != -1) {
        val score = cand.get("cprob").asInstanceOf[Double]
        candidates += ((wpid, score))
      }
    }
    val finalCandidates = candidates.groupBy(_._1).map(x => (x._1, x._2.map(_._2).sum)).toSeq.sortBy(-_._2).toSeq.take(numCandidates).toMap
    if (useFiger) {
      val figerTypesProbs: Map[String, Double] = getFigerTypesProbs(mention)
      //      println(figerTypesProbs)
      if (figerTypesProbs.isEmpty) {
        finalCandidates.map(c => (getFreebaseIdFromWpid(c._1), c._2)).filter(c => c._1 != null)
      } else {
        val candTypes: Map[Int, Set[String]] = getCandidateTypes(finalCandidates.map(_._1).toSeq)
        //      println(candTypes)
        val typeSumProbs: Map[String, Double] = figerTypesProbs.map(p => (p._1, candTypes.map(c => if (c._2.contains(p._1)) finalCandidates(c._1) else 0).sum)).toMap
        //      println(typeSumProbs)
        val adjustedCandidates = finalCandidates.map(c => {
          val types = candTypes(c._1)
          val prior = c._2
          val score = types.map(t => {
            val tsc = figerTypesProbs.getOrElse(t, 0.0)
            if (tsc != 0.0) {
              if (typeSumProbs(t) == 0.0)
                tsc
              else
                tsc * prior / typeSumProbs(t)
            } else {
              tsc
            }
          }).sum
          (c._1, score)
        })
        adjustedCandidates.map(c => (getFreebaseIdFromWpid(c._1), c._2)).filter(c => c._1 != null)
      }
    }
    else {
      finalCandidates.map(c => (getFreebaseIdFromWpid(c._1), c._2)).filter(c => c._1 != null)
    }
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

  def isNE(m: Mention, s: Sentence): Boolean = {
    !s.tokens.subList(m.toks._1 - 1, m.toks._2 - 1).exists(t => t.ner.getOrElse("") == "O")
  }

  override def process(doc: Document): Document = {
    print("linking doc=" + doc.id)
    for (e <- doc.entities) {
      e.freebaseIds.clear()
      print(".")
      // find the best name for linking
      //      println(e.toCase.toString)
      //      println("rep = "+e.representativeString+","+e.representativeMId)
      //      println(doc.mentions(e.representativeMId).toCase)
      var string = e.representativeString

      var repMention: Mention = null
      var realMention: Mention = null
      val sent = doc.sentences.find(s => s.mentions.exists(m => if (m.id == e.representativeMId) {
        repMention = m
        string = repMention.text
        //        println("string="+string)
        true
      } else false))

      realMention = repMention
      //      println("sent="+sent.get.toCase.toString)
      var needLink = false

      sent match {
        case Some(s) => needLink = isNE(repMention, s);
          for (m <- s.mentions) {
            if (repMention != m && m.toks._1 >= repMention.toks._1 && m.toks._2 <= repMention.toks._2 && isNE(m, s)) {
              // found a better mention for linking
              println("replace the string (" + string + ") by " + m.text)
              string = m.text
              realMention = m
              needLink = true
            }
          }
        case None => println("repMention not found in any sentence, mention=" + e.representativeMId + ", entity=" + e.id + ", doc=" + doc.id)
      }
      //      println("candidates for "+string)
      //      val fbIds = dictionary.getOrElse(string, Seq.empty)
      if (needLink) {
        val fbIds = getCandidates(realMention)
        e.freebaseIds ++= fbIds
      } else {
        e.freebaseIds ++= Map()
      }
    }
    println()
    doc
  }

  def getCandidateTypes(candidates: Seq[Int]): Map[Int, Set[String]] = {
    candidates.map(wpid => wpid -> getCandidateTypes(wpid)).toMap
  }

  def getCandidateTypes(candidate: Int): Set[String] = {
    val result = wpidColl.findOne(new BasicDBObject("wpid", candidate))
    if (result == null) {
      Set.empty
    } else {
      val types = result.get("fbtypes")
      if (types == null) {
        Set.empty
      } else {
        types.asInstanceOf[java.util.List[Object]].map(x => MapType.getMappedTypes(x.toString)).filter(_ != null).toSet
      }
    }
  }

  def getFigerTypesProbs(m: Mention): Map[String, Double] = {
    m.attrs.get("figer") match {
      case Some(pred) =>
        if (pred == "")
          Map.empty
        else {
          val figerTypes = pred.split("[,\t]").map(str => {
            val pair = str.split("@");
            println("pair = " + str)
            (pair(0), pair(1).toDouble)
          })
          val maxScore = figerTypes.map(_._2).max
          val Z = figerTypes.map(t => Math.exp(t._2)).sum
          figerTypes.map(t => (t._1 -> Math.exp(t._2) / Z)).toMap
        }
      case None => Map.empty
    }
  }

}

object TestCrosswikisLinker {
  def main(args: Array[String]): Unit = {
    val linker = new CrosswikiLinkerMongo(dbHost = "rv-n12", dbName = "vinculum", useFiger = true)
    val wpid = linker.getWpidFromTitle("Barack Obama")
    println(wpid)
    val mid = linker.getFreebaseIdFromWpid(wpid)
    println(mid)
    val mention = new Mention()
    mention.text = "Washington"
    mention.attrs += ("figer" -> "/person@1,/person/politician@0.5")
    val candidates = linker.getCandidates(mention)
    candidates.foreach(println)

    //    println(linker.getCandidateTypes(28957983))

    linker.close()
  }
}

object RunCrosswikisLinkerForD2D {
  def main(args: Array[String]): Unit = {
    val linker = new CrosswikiLinkerMongo(dbHost = "rv-n12", dbName = "vinculum", useFiger = true)
    val reader = new PerLineJsonReader(true)
    val docs = reader.read("nigeria_dataset_v04.nlp.lrf.json.gz")
    val nlpDocs = linker.process(docs)
    nlpDocs.foreach(println)
    val writer = new PerLineJsonWriter(true)
    writer.write("nigeria_dataset_v04.nlp.lrfl.json.gz", nlpDocs)
  }
}