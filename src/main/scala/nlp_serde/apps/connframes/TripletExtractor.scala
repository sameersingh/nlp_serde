package nlp_serde.apps.connframes

import java.io.{File, FilenameFilter}

import nlp_serde.{FileUtil, Util}
import nlp_serde.annotators.NamedEntityMentionAnnotator
import nlp_serde.immutable.{Document, Sentence, Token}
import nlp_serde.readers.PerLineJsonReader

import scala.collection.mutable.ArrayBuffer

/**
  * Created by sameer on 12/16/15.
  */
case class Triplet(subj: Seq[Token], verb: Token, obj: Seq[Token])

trait TripletExtractor {
  def fromSentence(sentence: Sentence): Seq[Triplet]

  def fromDoc(doc: Document): Seq[Triplet] = {
    doc.sentences.flatMap(s => fromSentence(s))
  }
}

object TripletExtractor {

  def tokenToMention(t: Token, s: Sentence): Seq[Token] = {
    val tidx = s.tokens.indexOf(t) + 1
    val mentions = s.mentions.filter(m => m.toks._1 <= tidx && m.toks._2 > tidx)
    if(mentions.size == 0) {
      Seq(t)
    } else {
      assert(mentions.size == 1, mentions.mkString(", "))
      s.tokens.slice(mentions.head.toks._1-1, mentions.head.toks._2-1)
    }
  }

  def expandTriplet(t: Triplet, s: Sentence): Triplet = {
    assert(t.subj.length == 1, t.subj)
    assert(t.obj.length == 1, t.obj)
    Triplet(TripletExtractor.tokenToMention(t.subj.head, s), t.verb,
      TripletExtractor.tokenToMention(t.obj.head, s))
  }
}

class DepParsingExtractor extends TripletExtractor {
  override def fromSentence(sentence: Sentence): Seq[Triplet] = {
    val result = new ArrayBuffer[Triplet]()
    val verbs = sentence.tokens.zipWithIndex.filter(p => p._1.pos.get.startsWith("V"))
    for ((v, idx) <- verbs) {
      // val idx = v.idx
      val targets = sentence.deps.get.filter(_.source == idx)
      for(s <- targets.filter(_.label == "nsubj")) {
        for(o <- targets.filter(_.label == "dobj")) {
          val t = Triplet(Seq(sentence.tokens(s.target-1)), v,
            Seq(sentence.tokens(o.target-1)))
          result += TripletExtractor.expandTriplet(t, sentence)
        }
      }
    }
    // if(result.size > 0) println("res: " + result.size)
    result
  }
}

object TripletExtractorTest {
  def main(args: Array[String]): Unit = {
    import nlp_serde.annotators.StanfordAnnotator

    val extr = new DepParsingExtractor
    val annot = new StanfordAnnotator(Seq("tokenize", "ssplit", "pos", "lemma", "ner", "parse"))
    val ner2mention = new NamedEntityMentionAnnotator
    val string = "John Smith greeted Brenda."
    val doc = new nlp_serde.Document(Document("doc", string))
    annot.process(doc)
    ner2mention.process(doc)

    for(s <- doc.sentences) {
      println(s.mentions.map(_.toCase).mkString("\n"))
      println(s.tokens.map(_.toCase).mkString("\n"))
      val trips = extr.fromSentence(s.toCase)
      println(trips.mkString("\n"))
    }
  }
}

object TripletExtractorApp {
  def main(args: Array[String]): Unit = {
    val outputFile = args.lift(0).getOrElse("data/nyt/triplets.txt.gz")
    val extr = new DepParsingExtractor
    val reader = new PerLineJsonReader(true)
    val ner2mention = new NamedEntityMentionAnnotator
    val writer = FileUtil.writer(outputFile, true)
    val docs = reader.readDir("data/nyt", new FilenameFilter {
      override def accept(file: File, s: String): Boolean = s.endsWith(".nlp.json.gz")
    })
    for(d <- docs) {
      ner2mention.process(d)
      val date = d.attrs("date")
      for(t <- extr.fromDoc(d.toCase)) {
        // println("%s\t%s\t%s\t%s".format(date, t.subj.map(_.text).mkString(" "), t.verb.text,
        //  t.obj.map(_.text).mkString(" ")))
        writer.println("%s\t%s\t%s\t%s".format(date, t.subj.map(_.text).mkString(" "), t.verb.lemma.get,
          t.obj.map(_.text).mkString(" ")))
      }
      writer.flush()
    }
    writer.flush()
    writer.close()
  }
}