package nlp_serde.annotators.relations

import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.BasicDependenciesAnnotation
import edu.stanford.nlp.util.CoreMap
import nlp_serde.writers.PerLineJsonWriter
import nlp_serde.{Sentence, Mention, Relation, Document}
import nlp_serde.annotators.{StanfordAnnotator, Annotator}
import nlp_serde.readers.PerLineJsonReader

import scala.collection.mutable.ArrayBuffer
import edu.washington.multirframework.featuregeneration.{FeatureGenerator, DefaultFeatureGeneratorMinusDirMinusDep}
import edu.washington.multirframework.argumentidentification.{SententialInstanceGeneration, ArgumentIdentification, DefaultSententialInstanceGeneration, NERArgumentIdentification}
import edu.washington.multirframework.multiralgorithm._
import scala.collection.mutable
import edu.stanford.nlp.pipeline.Annotation
import edu.washington.multir.preprocess.CorpusPreprocessing
import edu.washington.multirframework.data.{KBArgument, Argument}
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.ling.CoreAnnotations._
import java.util.Arrays
import java.util
import com.typesafe.config.ConfigFactory
import java.io.{File, FileOutputStream, OutputStreamWriter, PrintWriter}
import scala.collection.JavaConversions._

/**
 * Need to install Multir locally
 *mvn install:install-file \
-Dfile=multirexperiments_2.10-0.1.jar \
-DgroupId=multirexperiments \
-DartifactId=multirexperiments \
-Dversion=0.1 \
-Dpackaging=jar \
-DgeneratePom=true
 * Created by xiaoling on 11/4/14.
 */
class MultiRAnnotator(val pathToMultirFiles: String,
                      val fg: FeatureGenerator = new DefaultFeatureGeneratorMinusDirMinusDep,
                      val ai: ArgumentIdentification = NERArgumentIdentification.getInstance,
                      val sig: SententialInstanceGeneration = DefaultSententialInstanceGeneration.getInstance,
                      val arg1Type: String = "PERSON",
                      val arg2Type: String = "PERSON"
                       ) extends Annotator {
  private var mapping: Mappings = null
  private var model: Model = null
  private var params: Parameters = null
  private var scorer: Scorer = null
  private val relID2rel = new mutable.HashMap[Integer, String]

  def init() = {
    val dir = pathToMultirFiles
    try {
      mapping = new Mappings
      mapping.read(dir + "/mapping")
      model = new Model
      model.read(dir + "/model")
      params = new Parameters
      params.model = model
      params.deserialize(dir + "/params")
      scorer = new Scorer
      for (key <- mapping.getRel2RelID.keySet) {
        val id: Integer = mapping.getRel2RelID.get(key)
        relID2rel.put(id, key)
      }
    }
    catch {
      case e: Exception => {
        e.printStackTrace
      }
    }
  }

  init()

  case class RelationMention(arg1: Argument, arg2: Argument, relation: String, score: Double, senText: String) {
    def toFormattedString: String =
      "%s|||%d|||%d|||%s|||%s|||%d|||%d|||%f" format(
        arg1.getArgName, arg1.getStartOffset, arg1.getEndOffset,
        relation, //.replaceAll("|","___"),
        arg2.getArgName, arg2.getStartOffset, arg2.getEndOffset, score)
  }

  def extractFromText(text: String, name: String = "default"): Seq[RelationMention] = {

    val doc: Annotation = CorpusPreprocessing.getTestDocumentFromRawString(text, name)
    val extractions = new ArrayBuffer[RelationMention]
    val sentences = doc.get(classOf[CoreAnnotations.SentencesAnnotation])

    for (s <- sentences) {
      val senText: String = s.get(classOf[CoreAnnotations.TextAnnotation])
      val args = ai.identifyArguments(doc, s)
      val sigs = sig.generateSententialInstances(args, s)
      for (p <- sigs) {
        val arg1: Argument = p.first
        val arg2: Argument = p.second
        var arg1ID: String = null
        var arg2ID: String = null
        if (p.first.isInstanceOf[KBArgument]) {
          arg1ID = (p.first.asInstanceOf[KBArgument]).getKbId
        }
        if (p.second.isInstanceOf[KBArgument]) {
          arg2ID = (p.second.asInstanceOf[KBArgument]).getKbId
        }
        val features = fg.generateFeatures(arg1.getStartOffset, arg1.getEndOffset, arg2.getStartOffset, arg2.getEndOffset, arg1ID, arg2ID, s, doc)

        println("ORIGINAL FEATURES!" + features);

        val result = getPrediction(features.toList, arg1.getArgName, arg2.getArgName, senText)
        if (result != null) {
          val relationScoreTriple: Triple[String, Double, Double] = getPrediction(features.toList, arg1.getArgName, arg2.getArgName, senText)._1
          //val extractionString: String = arg1.getArgName + " " + relationScoreTriple._1 + " " + arg2.getArgName + "\n" + senText
          if (relationScoreTriple._1 != "NA")
            extractions.add(RelationMention(arg1, arg2, relationScoreTriple._1, relationScoreTriple._3, senText))
          //extractions.add(new Pair[String, Double](extractionString, relationScoreTriple._3))
        }
      }
    }

    for (e <- extractions) {
      val extrString: String = e.arg1.getArgName + " " + e.relation + " " + e.arg2.getArgName + "\n" + e.senText
      val score: Double = e.score
      System.out.println(extrString + "\t" + score)
    }

    extractions
  }

  /**
   * Conver features and args to MILDoc
   * and run Multir sentential extraction
   * algorithm, return null if no extraction
   * was predicted.
   * @param features
   * @param arg1name
   * @param arg2name
   * @return
   */
  private def getPrediction(features: List[String], arg1name: String, arg2name: String, senText: String): Pair[Triple[String, Double, Double], util.Map[Integer, java.lang.Double]] = {

    val doc: MILDocument = new MILDocument
    doc.arg1 = arg1name
    doc.arg2 = arg2name
    doc.Y = new Array[Int](1)
    doc.numMentions = 1
    doc.setCapacity(1)
    val sv: SparseBinaryVector = new SparseBinaryVector
    doc.features(0) = sv
    val ftrset: util.SortedSet[Integer] = new util.TreeSet[Integer]
    var totalfeatures: Int = 0
    var featuresInMap: Int = 0
    for (f <- features) {
      totalfeatures += 1
      val ftrid: Int = mapping.getFeatureID(f, false)
      if (ftrid >= 0) {
        featuresInMap += 1
        ftrset.add(ftrid)
      }
    }
    sv.num = ftrset.size
    sv.ids = new Array[Int](sv.num)
    var k: Int = 0
    for (f <- ftrset) {
      sv.ids(({
        k += 1;
        k - 1
      })) = f
    }
    var relation: String = ""
    var conf: Double = 0.0
    val mentionFeatureScoreMap = new java.util.HashMap[Integer, java.util.Map[Integer, java.lang.Double]]
    val parse: Parse = FullInference.infer(doc, scorer, params, mentionFeatureScoreMap)
    val Yp: Array[Int] = parse.Y
    if (parse.Z(0) > 0) {
      relation = relID2rel(parse.Z(0))
      Arrays.sort(parse.allScores(0))
      var combinedScore: Double = parse.score
      var i: Int = 0
      while (i < parse.allScores(0).length - 1) {
        {
          val s: Double = parse.allScores(0)(i)
          if (s > 0.0) {
            combinedScore += s
          }
        }
        ({
          i += 1;
          i - 1
        })
      }
      var confidence: Double = if ((combinedScore <= 0.0 || parse.score <= 0.0)) .1 else (parse.score / combinedScore)
      if (combinedScore == parse.score && parse.score > 0.0) {
        confidence = .001
      }
      conf = confidence
    } else {
      val negMentionFeatureScoreMap = new java.util.HashMap[Integer, java.util.Map[Integer, java.lang.Double]]
      val negParse: Parse = FullInference.infer(doc, scorer, params, negMentionFeatureScoreMap, 0)
      val t = Triple("NA", conf, parse.score)

      val p = (t -> negMentionFeatureScoreMap.get(0))
      return p
    }
    val t = new Triple[String, Double, Double](relation, conf, parse.score)
    val p = (t -> mentionFeatureScoreMap.get(0))
    return p
  }

  def extractFromDoc(fid: String, baseDir: String, filterSent: Int => Boolean = s => true) {
    val dname = "%s/processed/%s.sent" format(baseDir, fid)
    val output = "%s/processed/%s.rels" format(baseDir, fid)
    val writer = new PrintWriter(new OutputStreamWriter(new FileOutputStream(output), "UTF-8"))
    val input = scala.io.Source.fromFile(dname, "UTF-8")
    var sentId = 0
    for (s <- input.getLines()) {
      val extrs = if (filterSent(sentId)) extractFromText(s) else Seq.empty[RelationMention]
      if (extrs.map(_.senText).distinct.size > 1)
        println("Multiple sentences in {%s}: %s" format(s, extrs.mkString(", ")))
      writer.println(s + "\t" + extrs.map(_.toFormattedString).mkString("\t"))
      sentId += 1
    }
    input.close()
    writer.flush()
    writer.close()
  }

  def isNE(m: Mention, s: Sentence): Boolean = {
    !s.tokens.subList(m.toks._1 - 1, m.toks._2 - 1).exists(t => t.ner.getOrElse("") == "O")
  }

  override def process(doc: Document): Document = {
    val extractions = new ArrayBuffer[RelationMention]
    print("processing " + doc.id)
    for (s <- doc.sentences) {
      print(".")
      // sentential instance generation:
      // create an exhaustive list of pairs
      // filter out pairs of non-NE mentions (i.e. the mention has a O tag)
      // filter out pairs of the same mention
      // filter out pairs of mentions whose entity ids are the same if present
      val nes = s.mentions.filter(m => isNE(m, s))
      val pairs = nes.map(x => nes.map(y => (x, y))).flatten
      val sigs = pairs.filter(p => p._1 != p._2 && p._1.entityId.getOrElse(-1) != p._2.entityId.getOrElse(-2)
        && p._1.ner.getOrElse("") == arg1Type && p._2.ner.getOrElse("") == arg2Type)
      for (p <- sigs) {
        val arg1: nlp_serde.Mention = p._1
        val arg2: nlp_serde.Mention = p._2

        //  val features = fg.generateFeatures(arg1.getStartOffset, arg1.getEndOffset, arg2.getStartOffset, arg2.getEndOffset, arg1ID, arg2ID, s, doc)
        val features = SerdeDefaultMultiRFeatureGeneratorMinusDirMinusDep.generateFeatures(arg1, arg2, s)
//        println("CONVERTED FEATURES!" + features);
        val result = getPrediction(features.toList, arg1.text, arg2.text, s.text)
        if (result != null) {
          val relationScoreTriple: Triple[String, Double, Double] = result._1
          if (relationScoreTriple._1 != "NA") {
            //            println("arg1:" + arg1.toCase)
            //            println("arg2:" + arg2.toCase)
            //            println("sent:" + s.tokens.size)
            extractions.add(RelationMention(new Argument(arg1.text, s.tokens(arg1.toks._1 - 1).chars._1, s.tokens(arg1.toks._2 - 1 - 1).chars._2),
              new Argument(arg2.text, s.tokens(arg2.toks._1 - 1).chars._1, s.tokens(arg2.toks._2 - 1 - 1).chars._2),
              relationScoreTriple._1, relationScoreTriple._3, s.text))
            val rel = new Relation()
            rel.m1Id = arg1.id
            rel.m2Id = arg2.id
            rel.relations += relationScoreTriple._1
            s.relations += rel
          }
        }
      }
    }
    for (e <- extractions) {
      val extrString: String = e.arg1.getArgName + " " + e.relation + " " + e.arg2.getArgName + "\n" + e.senText
      val score: Double = e.score
      System.out.println(extrString + "\t" + score)
    }
    println()
    doc
  }
}

class SerdeDefaultMultiRFeatureGeneratorMinusDirMinusDep {

}

object SerdeDefaultMultiRFeatureGeneratorMinusDirMinusDep {
  def generateFeatures(arg1: nlp_serde.Mention, arg2: nlp_serde.Mention, s: nlp_serde.Sentence): List[String] = {
    import scala.collection.JavaConverters._
    val tokenStrings = s.tokens.map(t => t.text).toArray
    val posTags = s.tokens.map(t => t.pos.get).toArray
    val arg1Pos = Array(arg1.toks._1 - 1, arg1.toks._2 - 1)
    val arg2Pos = Array(arg2.toks._1 - 1, arg2.toks._2 - 1)
    val arg1ner = arg1.ner.getOrElse("")
    val arg2ner = arg2.ner.getOrElse("")
    //initialize dependency parents to -1
    val depParents = Array.fill(tokenStrings.size)(-1)
    val depTypes = Array.ofDim[String](tokenStrings.size)

    //dependency conversions..
    if (s.depTree.isEmpty)
      List[String]()
    else {
      for (dep <- s.depTree.get) {
        val child = dep.target - 1
        val parent = if (dep.source - 1 == child) -1 else dep.source - 1
        val depType = dep.label

        if (child < s.tokens.size) {
          depParents(child) = parent
          depTypes(child) = depType
        } else {
          System.err.println("ERROR BETWEEN DEPENDENCY PARSE AND TOKEN SIZE");
          return List[String]()
        }
      }
      //      for(Triple<Integer,String,Integer> dep : dependencyData){
      //        int parent = dep.first -1;
      //        String type = dep.second;
      //        int child = dep.third -1;
      //        //child and parent should not be equivalent
      //        if(parent == child){
      //          parent = -1;
      //        }
      //        if(child < tokens.size()){
      //          depParents[child] = parent;
      //          depTypes[child] = type;
      //        }
      //        else{
      //          System.err.println("ERROR BETWEEN DEPENDENCY PARSE AND TOKEN SIZE");
      //          return new ArrayList<String>();
      //        }
      //      }
      //    }
      DefaultFeatureGeneratorMinusDirMinusDep.originalMultirFeatures(tokenStrings, posTags, depParents, depTypes, arg1Pos, arg2Pos, arg1ner, arg2ner).toList
    }
  }
}

/**
 * Run MultiR with a partitioned model for the given pair of entity types
 * args:
 * 0 -> arg1 entity type
 * 1 -> arg2 entity type
 * 2 -> input file name
 * 3 -> output file name
 */
object RunPartitionedMultiRAnnotator extends App {
  args.foreach(println)
  val inputFile = args(2)
  val outputFile = args(3)

  val shortNames = Map("PERSON" -> "PER", "ORGANIZATION" -> "ORG", "LOCATION" -> "LOC", "MISC" -> "OTHER")
  val arg1Type = args(0)
  val arg2Type = args(1)

  val modelPath = ConfigFactory.load().getString("nlp.multir.modelPath") + "-" + shortNames(arg1Type) + shortNames(arg2Type) + "/"
  println(modelPath)
  val multir = new MultiRAnnotator(modelPath, arg1Type=arg1Type, arg2Type=arg2Type)

  val reader = new PerLineJsonReader(true)
  val docs = reader.read(inputFile)

  val nlpDocs = multir.process(docs)
  //  nlpDocs.foreach(println)
  val writer = new PerLineJsonWriter(true)
  writer.write(outputFile, nlpDocs)

  //  val d = new Document()
  //  d.id = "test"
  //  d.text = "Barack, the US president, is married to Michelle"
  //  val annotator: StanfordAnnotator = new StanfordAnnotator(collapsed = false)
  //  val nlpDoc = annotator.process(d)
  //  println(multir.process(d))
  //  println("==================================")
  //  println(multir.extractFromText("Barack, the US president, is married to Michelle.").mkString("\n"))
  //  Mahira gave Yakib the phone number of her fiancé, Safi Sultaan.

  //  val reader = new PerLineJsonReader()
  //  val d = reader.read("nigeria_dataset_v04.nlp.json.gz").next()
  //  println(d.id)
  //  println(multir.process(d))
}

/*object RunMultiRAnnotator extends App {
  val modelPath = ConfigFactory.load().getString("nlp.multir.modelPath")
  val baseDir = ConfigFactory.load().getString("nlp.data.baseDir")
  val filelist = ConfigFactory.load().getString("nlp.data.filelist")
  println(modelPath)
  val multir = new MultiRAnnotator(modelPath)

  println("Reading processed documents")
  val reader = new ReadProcessedDocs(baseDir, filelist)
  val (db, einfo) = reader.readAllDocs

  println("Running relation extraction")
  val fileList = io.Source.fromFile(baseDir + "/" +  filelist, "UTF-8")
  for (line <- fileList.getLines();
       fid = line.split("\t")(0).dropRight(4)) {
    println("doc: " + fid)
    multir.extractFromDoc(fid, baseDir, (s: Int) => einfo.sentences.getOrElse(fid -> s, Seq.empty).size >= 2)
  }
  fileList.close()
}*/

/*
class ReadMultiROutput(val baseDir: String, val filelist: String, val minScore: Double = Double.NegativeInfinity) {

  case class Mention(string: String, start: Int, end: Int)

  case class RelationMention(relation: String, arg1: Mention, arg2: Mention, score: Double)

  var numDirectErrors = 0
  var numSearchErrors = 0
  var numTotalRequests = 0

  private def findClosestProvenance(db: DB, m: Mention, ps: Seq[(String, Provenance)]): Option[(String, Provenance)] = {
    numTotalRequests += 1
    def dist(m: Mention, p: Provenance): Double = math.pow(m.start - p.tokPos(0)._1, 2) + math.pow(m.end - p.tokPos(0)._2, 2)
    val sp = ps.minBy(sp => dist(m, sp._2))
    val d = dist(m, sp._2)
    if (d > 0.0) {
      val sent = db.sentence(sp._2.docId, sp._2.sentId)
      numDirectErrors += 1
      //println(m + "\t" + sent + "\n\t" + ps.map(_._2.tokPos(0)).map(se => se + ":" + sent.substring(se._1, se._2)).mkString("\t"))
      // now try searching
      val result = ps.find(sp => m.string == sent.string.substring(sp._2.tokPos(0)._1,sp._2.tokPos(0)._2))
      if(result.isEmpty) numSearchErrors += 1
      result
    } else Some(sp)
  }

  def multirRelation(r: String): String = if(r.contains("/")) {
    val init = r.drop(1)
    val firstSlash = init.indexOf("/")
    val lastSlash = init.lastIndexOf("/")
    init.substring(0, firstSlash) + "_" + init.substring(lastSlash + 1)
  } else r

  def updateFromDoc(fid: String, db: InMemoryDB) {
    val dname = "%s/processed/%s.rels" format(baseDir, fid)
    if (!new File(dname).exists()) return
    val input = io.Source.fromFile(dname, "UTF-8")
    var sentId = 0
    for (s <- input.getLines()) {
      val split = s.split("\\t").drop(1) // drop the sentence text
      val rms = for (rmstr <- split) yield {
          // NDLEA|||58|||63|||/organization/organization/headquarters|/location/mailing_address/citytown|||Lagos|||117|||122|||138884194.000000
          val rmSplit = rmstr.split("\\|\\|\\|")
          assert(rmSplit.length == 8)
          RelationMention(multirRelation(rmSplit(3)),
            Mention(rmSplit(0), rmSplit(1).toInt, rmSplit(2).toInt),
            Mention(rmSplit(4), rmSplit(5).toInt, rmSplit(6).toInt), rmSplit(7).toDouble)
        }
      val trueProvenances = db.docEntityProvenances(fid, sentId).map({
        case (mid, ps) => ps.map(mid -> _)
      }).flatten.toSeq
      for (rm <- rms) {
        val a1 = findClosestProvenance(db, rm.arg1, trueProvenances)
        val a2 = findClosestProvenance(db, rm.arg2, trueProvenances)
        for (arg1P <- a1; arg2P <- a2) {
          val p = Provenance(fid, sentId, arg1P._2.tokPos ++ arg2P._2.tokPos, rm.score)
          // add to db
          val rid = if(arg1P._1 < arg2P._1) arg1P._1 -> arg2P._1 else arg2P._1 -> arg1P._1
          val rel = rm.relation
          db._relationPredictions(rid) = db._relationPredictions.getOrElse(rid, Set.empty) ++ Seq(rel)
          val rt = db._relationText.getOrElse(rid, RelationText(rid._1, rid._2, Seq.empty))
          db._relationText(rid) = RelationText(rt.sourceId, rt.targetId, rt.provenances ++ Seq(p))
          val rmp = db._relationProvenances.getOrElseUpdate(rid, new mutable.HashMap).getOrElseUpdate(rel, RelModelProvenances(rid._1, rid._2, rel, Seq.empty))
          db._relationProvenances(rid)(rel) = RelModelProvenances(rmp.sourceId, rmp.targetId, rmp.relType, rmp.provenances ++ Seq(p))
        }
      }
      sentId += 1
    }
    input.close()
  }

  def updateFromAllDocs(db: InMemoryDB) {
    println("Running relation extraction")
    val fileList = io.Source.fromFile(baseDir + "/" + filelist, "UTF-8")
    var numRead = 0
    for (line <- fileList.getLines();
         fid = line.split("\t")(0).dropRight(4)) {
      // println("doc: " + fid)
      updateFromDoc(fid, db)
      if (numRead % 79 == 0) print(".")
      numRead += 1
    }
    fileList.close()
    println
    println(s"numDirect: $numDirectErrors, numSearch: $numSearchErrors, total: $numTotalRequests")
  }

}


*/
