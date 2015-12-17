package nlp_serde.apps.connframes

import nlp_serde.annotators.NamedEntityMentionAnnotator
import nlp_serde.immutable.{Document, Sentence, Token}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by sameer on 12/16/15.
  */
case class Triplet(subj: Seq[Token], verb: Token, obj: Seq[Token])

trait TripletExtractor {
  def fromSentence(sentence: Sentence): Seq[Triplet]
}

object TripletExtractor {

  def tokenToMention(t: Token, s: Sentence): Seq[Token] = {
    val mentions = s.mentions.filter(m => m.toks._1 <= t.idx && m.toks._2 > t.idx)
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
    val verbs = sentence.tokens.filter(_.pos.get.startsWith("V"))
    for (v <- verbs) {
      val idx = v.idx
      val targets = sentence.deps.get.filter(_.source == idx)
      for(s <- targets.filter(_.label == "nsubj")) {
        for(o <- targets.filter(_.label == "dobj")) {
          val t = Triplet(Seq(sentence.tokens(s.target-1)), v,
            Seq(sentence.tokens(o.target-1)))
          result += TripletExtractor.expandTriplet(t, sentence)
        }
      }
    }
    result
  }
}

object TripletExtractorApp {
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