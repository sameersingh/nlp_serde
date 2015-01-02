package nlp_serde.annotators

import java.text.Normalizer

import com.mongodb.{BasicDBObject, MongoClient, DB, DBCollection}
import edu.washington.cs.figer.analysis.MapType
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.PerLineJsonWriter
import nlp_serde.{Entity, Sentence, Mention, Document}

import scala.collection.mutable.ArrayBuffer
import scala.collection.JavaConversions._

/**
 *
 * need to launch vinculum mongodb first
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

  /**
   * normalize the string for crosswikis query
   * @param string
   * @return
   */
  def getLnrm(string: String): String = {
    var lnrm = Normalizer.normalize(string, Normalizer.Form.NFD)
    lnrm = lnrm.replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    lnrm = lnrm.toLowerCase()
    lnrm = lnrm.replaceAll("[^\\p{Alnum}]+", "")
    lnrm
  }

  val sortByQuery = new BasicDBObject("cprob", -1)

  def getCandidates(mention: Mention): Map[String, Double] = {
    val candidates = new ArrayBuffer[(Int, Double)]
    val lrnm = getLnrm(mention.text)
    val cursor = xwikisColl.find(new BasicDBObject("anchor", lrnm)) //.sort(sortByQuery).limit(numCandidates * 2)
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
      // if figer is involved, fetch the candidates' entity types and reranking the candidates based on the mention's figer predictions
      // TODO if the mention is newly created, the figer predictions need to generated on-the-fly
      val figerTypesProbs: Map[String, Double] = getFigerTypesProbs(mention)
      if (figerTypesProbs.isEmpty) {
        finalCandidates.map(c => (getFreebaseIdFromWpid(c._1), c._2)).filter(c => c._1 != null)
      } else {
        val candTypes: Map[Int, Set[String]] = getCandidateTypes(finalCandidates.map(_._1).toSeq)
        val typeSumProbs: Map[String, Double] = figerTypesProbs.map(p => (p._1, candTypes.map(c => if (c._2.contains(p._1)) finalCandidates(c._1) else 0).sum)).toMap
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

  val neClasses = scala.collection.immutable.HashSet("LOCATION", "PERSON", "ORGANIZATION", "MISC")

  /**
   * A P/L/O/M mention with all tokens in the same ner type
   * @param m
   * @param s
   * @return
   */
  def isNE(m: Mention, s: Sentence): Boolean = {
    m.mentionType.getOrElse("") == "PROPER" && !s.tokens.subList(m.toks._1 - 1, m.toks._2 - 1).exists(t => t.ner.getOrElse("") == "O") && neClasses.contains(m.ner.getOrElse(""))
  }

  override def process(doc: Document): Document = {
    print("linking doc=" + doc.id)
      for (e <- doc.entities) {
        e.freebaseIds.clear()
        print(".")
        // find the best name for linking
        var repMention: Mention = null
        var realMention: Mention = null
        val sent = doc.sentences.find(s => s.mentions.exists(m => if (m.id == e.representativeMId) {
          repMention = m
          true
        } else false))

        realMention = repMention
        //      println("sent="+sent.get.toCase.toString)
        var needLink = false
        sent match {
          case Some(s) => needLink = isNE(repMention, s);
            if (!needLink && repMention.mentionType.getOrElse("") == "PROPER" && neClasses.contains(repMention.ner.getOrElse(""))) {
              for (m <- s.mentions) {
                if (repMention != m && e.id == m.entityId.getOrElse(-1) && m.toks._1 >= repMention.toks._1 && m.toks._2 <= repMention.toks._2 && isNE(m, s)) {
                  // found a better mention for linking
                  println("replace the string (" + repMention.text + ") by " + m.text)
                  realMention = m
                  needLink = true
                }
              }
              // if still no luck with a reasonable mention, consider create a submention spanning from the head
              if (!needLink) {
                //        if (repMention.mentionType.getOrElse("") == "PROPER") {
                //          // meaning that we can't find a submention that is purely a named entity
                //          // use other mentions in the same cluster
                //          //          val otherMention = getOtherMention(doc, e)
                //          // get a sub mention which matches the type
                //          val otherMention = getSubMention(doc, e, repMention)
                //          if (otherMention != null) {
                //            realMention = otherMention
                //            needLink = true
                //          }
                //        }
                val otherMention = getSubMention(doc, s, repMention)
                if (otherMention != null) {
                  println("replace the string (" + repMention + ") by a submention " + otherMention.text)
                  realMention = otherMention
                  needLink = true
                }
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

  def getSubMention(doc: Document, s: Sentence, repMention: Mention): Mention = {
    val nerType = repMention.ner.get
    if (s.tokens(repMention.headTokenIdx - 1).ner.getOrElse("") == nerType) {
      //      repMention.s.tokens.subList(m.toks._1 - 1, m.toks._2 - 1)
      val left = ((repMention.toks._1 - 1) until (repMention.headTokenIdx)).filter(i => s.tokens(i).ner.getOrElse("") == nerType).min
      val right = ((repMention.headTokenIdx - 1) until (repMention.toks._2 - 1)).filter(i => s.tokens(i).ner.getOrElse("") == nerType).max
      val m = new Mention()
      m.entityId = repMention.entityId
      m.headTokenIdx = repMention.headTokenIdx
      m.mentionType = repMention.mentionType
      m.ner = repMention.ner
      m.toks = (left + 1, right + 2)
      m.sentenceId = repMention.sentenceId
      m.text = (m.toks._1 - 1 until m.toks._2 - 1).map(i => s.tokens(i).text).mkString(" ")
      m
    } else {
      null
    }
  }

  /**
   * An alternative approach to finding a mention for linking from the coref cluster
   * @param doc
   * @param e
   * @return
   */
  def getOtherMention(doc: Document, e: Entity): Mention = {
    val mentions = doc.mentions
    for (m <- e.mids.toSeq.sorted) {
      if (e.representativeMId != m) {
        val men = mentions(m)
        val msent = doc.sentences.find(s => s.mentions.exists(mm => if (mm.id == m) {
          //                repMention = m
          //                  string = repMention.text
          true
        } else false))
        msent match {
          case Some(s) =>
            if (men.mentionType.getOrElse("") == "PROPER" && isNE(men, s)) {
              return men
            }
          case None => println("repMention not found in any sentence, mention=" + e.representativeMId + ", entity=" + e.id + ", doc=" + doc.id)
        }
      }
    }
    return null
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
            //println("pair = " + str)
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
    linker.close()
  }
}

/**
 * args:
 * 0 -> input file name
 * 1 -> output file name
 * 2 -> (optional) doc offset start
 * 3 -> (optional) doc offset end
 */
object RunCrosswikisLinkerForD2D {
  def main(args: Array[String]): Unit = {
    val input = args(0) // "nigeria_dataset_v04.nlp.lrf.json.gz";
    val output = args(1) // "nigeria_dataset_v04.nlp.lrfl.json.gz"
    val linker = new CrosswikiLinkerMongo(dbHost = "rv-n12", dbName = "vinculum", useFiger = true)
    val reader = new PerLineJsonReader(true)
    val docs = reader.read(input)
    val (s, e) = if (args.length > 2) {
      (args(2).toInt, args(3).toInt)
    } else (0, docs.length)

    val nlpDocs = linker.process(docs.drop(s).take(e - s))
    //    nlpDocs.foreach(println)
    val writer = new PerLineJsonWriter(true)
    writer.write(output, nlpDocs)
  }
}
