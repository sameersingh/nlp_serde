package nlp_serde.annotators


import java.util

import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.PerLineJsonWriter
//import edu.washington.cs.figer.FigerSystem
import edu.washington.cs.figer.data.EntityProtos.Mention
import edu.washington.cs.figer.data.EntityProtos.Mention.Dependency
import nlp_serde.Document
import nlp_serde.FileUtil
import nlp_serde.{FileUtil, Document}
import nlp_serde.FileUtil
import scala.collection.mutable.{HashMap, HashSet}
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream
import java.io.{InputStreamReader, BufferedReader, FileInputStream}
import java.util.zip.GZIPInputStream
import java.util.regex.Pattern

/**
 * Need to install FIGER locally first
 * mvn install:install-file \
-Dfile=figer.jar \
-DgroupId=figer \
-DartifactId=figer \
-Dversion=0 \
-Dpackaging=jar \
-DgeneratePom=true
 * Created by xiaoling on 11/8/14.
 */
class FigerAnnotator(modelFile: String, threshold: Double = 0.0) extends Annotator {
  //FigerSystem.modelFile = modelFile
  //lazy val figer: FigerSystem = FigerSystem.instance()

  override def process(doc: Document): Document = {
    import scala.collection.JavaConverters._
    for (s <- doc.sentences) {
      lazy val figerDeps = s.depTree.getOrElse(Seq.empty).map(dep =>
        Dependency.newBuilder().setType(dep.label).setGov(dep.source).setDep(dep.target).build()).toList.asJava
      lazy val tokens = s.tokens.map(tok => tok.text).toList.asJava
      lazy val postags = s.tokens.map(tok => tok.pos.get).toList.asJava
      for (m <- s.mentions) {
        if (!Range(m.toks._1 - 1, m.toks._2 - 1).exists(i => s.tokens(i).ner.getOrElse("O") == "O")) {
          // a true named entity mention
          val figerMention = Mention.newBuilder()
            .setStart(m.toks._1 - 1).setEnd(m.toks._2 - 1)
            .addAllTokens(tokens).addAllPosTags(postags).addAllDeps(figerDeps)
            .setEntityName("").setFileid("").setSentid(s.idx - 1).build()
          val features = new util.ArrayList[String]()
          //figer.nerFeature.extract(figerMention, features)
          // remove below threshold labels
          val pred = "" /*figer.predict(features).split("[,\t]").map(str => {
            val pair = str.split("@");
            (pair(0), pair(1).toDouble)
          }).filter(p => p._2 > threshold).map(p=>p._1+"@"+p._2.toString).mkString(",")*/
          m.attrs += ("figer" -> pred)
        }
      }
    }
    doc
  }
}


object FigerAnnotator {
  def main(args: Array[String]) {
    val modelFile = if (args.size > 0) args(0) else "../figer/test.model.gz"
    val stanf = new StanfordAnnotator()
    val figer = new FigerAnnotator(modelFile)
    val reader = new PerLineJsonReader(true)
    val docs = figer.process(reader.read("nigeria_dataset_v04.nlp.lr.json.gz"))
    val writer = new PerLineJsonWriter(true)
    writer.write("nigeria_dataset_v04.nlp.lrf.json.gz", docs)

//    val d = new Document()
//    d.id = "doc001"
//    d.text = "Barack Obama is the president of the United States. He is married to Michelle Obama, and is not related to George Bush."

//    stanf.process(d)
//    figer.process(d)
//    val pd = d.toCase
//    println(pd.entities.mkString("\n"))
//    println(pd.sentences.flatMap(_.mentions).mkString("\n"))
  }
}
