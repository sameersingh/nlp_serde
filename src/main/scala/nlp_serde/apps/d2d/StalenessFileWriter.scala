package nlp_serde.apps.d2d

import java.io.PrintWriter
import java.text.SimpleDateFormat
import java.util.Date

import nlp_serde.{Entity, Document}
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.{HTMLWriter, Writer}

import scala.collection.mutable

/**
 * Created by sameer on 11/6/14.
 */
class StalenessFileWriter extends Writer {
  override def write(name: String, docs: Iterator[Document]): Unit = {
    val writer = new PrintWriter(name)
    for (d <- docs) {
      write(writer, d)
    }
    writer.flush()
    writer.close()
  }

  case class Entry(id: String, toks: Set[String], date: Date)

  def entityId(e: Entity): Option[String] = if (e.ner.get != "O" && e.freebaseIds.size > 0) Some(e.freebaseIds.maxBy(_._2)._1) else None

  def entityEntries(d: Document): Seq[Entry] = {
    if (!d.attrs.contains("date")) return Seq.empty
    val f = new SimpleDateFormat("yyyy-MM-dd")
    val date = f.parse(d.attrs("date"))
    d.entities.filter(_.ner.isDefined).flatMap(e => {
      entityId(e).map(id => {
        val toks = d.sentences.flatMap(s => {
          s.mentions.filter(e.mids contains _.id).flatMap(m => {
            s.tokens.filter(t => {
              val pos = t.pos.getOrElse("O")
              (pos.startsWith("N") || pos.startsWith("V")) && !pos.startsWith("NNP")
            }).map(t => t.lemma.getOrElse(t.text))
          })
        }).toSet
        Entry(id, toks, date)
      }).toSeq
    })
  }

  def relationEntries(d: Document): Seq[Entry] = {
    if (!d.attrs.contains("date")) return Seq.empty
    val f = new SimpleDateFormat("yyyy-MM-dd")
    val date = f.parse(d.attrs("date"))
    val map = new mutable.HashMap[String, Entry]
    for (s <- d.sentences; r <- s.relations) {
      val m1 = s.mentions.find(_.id == r.m1Id).get
      val m2 = s.mentions.find(_.id == r.m2Id).get
      val e1 = d.entities.find(_.id == m1.entityId.get).get
      val e2 = d.entities.find(_.id == m2.entityId.get).get
      for (e1id <- entityId(e1); e2id <- entityId(e2)) {
        val rid = if (e1id < e2id) e1id + "|" + e2id else e2id + "|" + e1id
        val toks = s.tokens.filter(t => {
          val pos = t.pos.getOrElse("O")
          (pos.startsWith("N") || pos.startsWith("V")) && !pos.startsWith("NNP")
        }).map(t => t.lemma.getOrElse(t.text))
        val entry = map.getOrElse(rid, Entry(rid, Set.empty, date))
        map(rid) = Entry(rid, entry.toks ++ toks, date)
      }
    }
    map.values.toSeq
  }

  def write(writer: PrintWriter, d: Document): Unit = {
    val entries = entityEntries(d)
    println("Number of entities: " + entries.size)
    for (e <- entries) {
      writer.println("%s\t%s\t%s\t%d" format(d.id, e.id, e.toks.map(_.trim).mkString("|"), e.date.getTime))
    }
    val relEntries = relationEntries(d)
    println("Number of relations: " + entries.size)
    for (e <- relEntries) {
      writer.println("%s\t%s\t%s\t%d" format(d.id, e.id, e.toks.map(_.trim).mkString("|"), e.date.getTime))
    }
  }
}

object StalenessFileWriter {
  def main(args: Array[String]): Unit = {
    val baseDir = "/Users/sameer/Google Drive/UW/D2D/D2D Nov 14/" //"/home/sameer/data/d2d/demo2015/nov/"
    val inputFile = baseDir + "nigeria_dataset_v04.nlp.lrf.json.gz"
    val outputFile = baseDir + "nigeria_dataset_v04.staleness.txt"
    val reader = new PerLineJsonReader()
    val docs = reader.read(inputFile)
    val writer = new StalenessFileWriter()
    writer.write(outputFile, docs)
  }
}

object DocReader {
  def main(args: Array[String]) {
    val inputFile = "/home/sameer/data/d2d/demo2015/nov/nigeria_dataset_v04.nlp.cw.json.gz"
    val outputDir = "/home/sameer/data/d2d/demo2015/nov/html/"
    val writer = new HTMLWriter
    writer.write(outputDir, new PerLineJsonReader().read(inputFile))
  }
}