package nlp_serde.apps.d2d

import java.io.{FileOutputStream, FileInputStream, PrintWriter}
import java.text.SimpleDateFormat
import java.util.Date
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import nlp_serde.{FileUtil, Entity, Document}
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.{PerLineJsonWriter, HTMLWriter, Writer}

import scala.collection.mutable

/**
 * Created by sameer on 11/6/14.
 */
class StalenessFileWriter extends Writer {
  override def write(name: String, docs: Iterator[Document]): Unit = {
    val writer = FileUtil.writer(name, true)
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
    // println("Number of entities: " + entries.size)
    for (e <- entries) {
      writer.println("%s\t%s\t%s\t%d" format(d.id, e.id, e.toks.map(_.trim).mkString("|"), e.date.getTime))
    }
    val relEntries = relationEntries(d)
    // println("Number of relations: " + entries.size)
    for (e <- relEntries) {
      writer.println("%s\t%s\t%s\t%d" format(d.id, e.id, e.toks.map(_.trim).mkString("|"), e.date.getTime))
    }
  }
}

object StalenessFileWriter {
  def main(args: Array[String]): Unit = {
    val baseDir = "data/d2d/"
    val inputFile = baseDir + "docs.nlp.flr.json.gz"
    val outputFile = baseDir + "docs.staleness.txt.gz"
    val reader = new PerLineJsonReader()
    val docs = reader.read(inputFile)
    val writer = new StalenessFileWriter()
    writer.write(outputFile, docs)
  }
}

/**
 * Fix the bug in NERMentionAnnotator that did not link the entities to the mentions
 */
object FixMentionsAndEntities {
  def main(args: Array[String]): Unit = {
    val baseDir = "data/d2d/"
    val inputFile = baseDir + "docs.nlp.flr.json.gz"
    val outputFile = baseDir + "docs.nlp.flr.json.gz.fixed"
    val reader = new PerLineJsonReader(true)
    val docs = reader.read(inputFile)
    val writer = new PerLineJsonWriter(true)
    writer.write(outputFile, docs.map(d => {
      for(m <- d.mentions.values)
        m.entityId = Some(m.id)
      for(e <- d.entities)
        e.mids += e.id
      d
    }))
  }
}

object DocReader {
  def main(args: Array[String]) {
    val inputFile = "data/d2d/docs.txt.json.gz"
    val outputDir = "data/d2d/html/"
    val writer = new HTMLWriter
    writer.write(outputDir, new PerLineJsonReader().read(inputFile))
  }
}
