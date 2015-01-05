package nlp_serde.annotators

import nlp_serde.Document
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.PerLineJsonWriter

import scala.collection.JavaConversions._

/**
 * Named Entity Place Holder Annotator
 * creates a mention for each predicted named entity of certain types
 * and a corresponding entity for each mention.
 *
 * This annotator should run after StanfordAnnotator without dcoref.
 *
 * Created by xiaoling on 1/1/15.
 */
class NamedEntityMentionAnnotator extends Annotator {

  // only use entities of the following types
  lazy val validNerTypes = Set("PERSON", "ORGANIZATION", "LOCATION", "MISC")

  override def process(doc: Document): Document = {
    for (s <- doc.sentences) {
      val nerTokens = s.tokens.map(tok => tok.ner.get).zipWithIndex.filter(x => validNerTypes contains x._1)
      val startTokens = nerTokens.filter(x => !nerTokens.contains(x._1, x._2 - 1)).map(x => x._2)
      val endTokens = nerTokens.filter(x => !nerTokens.contains(x._1, x._2 + 1)).map(x => x._2 + 1)
      val spans = startTokens.zip(endTokens)
      val spanTags = nerTokens.filter(x => !nerTokens.contains(x._1, x._2 - 1)).map(x => x._1).zip(spans)
      for ((tag, span) <- spanTags) {
        val m = new nlp_serde.Mention()
        m.sentenceId = s.idx
        m.toks = (span._1 + 1, span._2 + 1)
        m.text = s.tokens.subList(m.toks._1 - 1, m.toks._2 - 1).map(_.text).mkString(" ")
        m.id = doc.entities.length + 1
        m.ner = Some(tag)
        m.mentionType = Some("PROPER")
        m.posInSentence = s.mentions.length + 1
        s.mentions += m
        val e = new nlp_serde.Entity()
        e.id = m.id
        e.representativeMId = m.id
        e.representativeString = m.text
        e.ner = Some(m.ner.get)
        doc.entities += e
      }
    }
    doc
  }
}

object NamedEntityMentionAnnotator {
  def main(args: Array[String]): Unit = {
    if (args.length < 2) {
      println("Not enough arguments: input_file.json.gz output_file.json.gz")
    }
    val input = args(0)
    val output = args(1)

    println("Reading: " + input)
    val reader = new PerLineJsonReader(true)
    val docs = reader.read(input)
    val limits =
      if (args.length == 4) {
        Some(args(2).toInt, args(3).toInt)
      } else {
        None
      }
    limits.foreach(l => "processing from %d to %d" format(l._1, l._2))
    //val annotator = new MultiThreadedAnnotator(new StanfordAnnotator(Seq("tokenize", "ssplit", "pos", "lemma", "ner", "parse"), false))
    //val nlpDocs = annotator.process(docs)
    val annotator = new NamedEntityMentionAnnotator
    val nlpDocs = if (limits.isDefined) {
      annotator.process(docs.drop(limits.get._1).take(limits.get._2 - limits.get._1))
    } else annotator.process(docs)
    println("Writing: " + output)
    val writer = new PerLineJsonWriter(true)
    if (limits.isDefined) {
      val (start, end) = limits.get
      writer.write(output + "-" + start + "-" + end, nlpDocs)
    } else writer.write(output, nlpDocs)
  }
}