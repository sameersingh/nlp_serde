package nlp_serde.apps.d2d

import java.io.PrintWriter
import java.text.SimpleDateFormat
import java.util.Date

import nlp_serde.Document
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.Writer

/**
 * Created by sameer on 11/6/14.
 */
class StalenessFileWriter extends Writer {
  override def write(name: String, docs: Iterator[Document]): Unit = {
    val writer = new PrintWriter(name)
    for(d <- docs) {
      write(writer, d)
    }
    writer.flush()
    writer.close()
  }

  case class Entry(id: String, toks: Set[String], date: Date)

  def entityEntries(d: Document): Seq[Entry] = {
    if(!d.attrs.contains("date")) return Seq.empty
    val f = new SimpleDateFormat("yyyy-MM-dd")
    val date = f.parse(d.attrs("date"))
    d.entities.filter(_.ner.isDefined).map(e => {
      val id = if(e.freebaseIds.size > 0) e.freebaseIds.maxBy(_._2)._1 else e.representativeString
      val toks = d.sentences.flatMap(s => {
        s.mentions.filter(e.mids contains _.id).flatMap(m => {
          s.tokens.filter(t => {
            val pos = t.pos.getOrElse("O")
            pos.startsWith("N") || pos.startsWith("V")
          }).map(t => t.lemma.getOrElse(t.text))
        })
      }).toSet
      Entry(id, toks, date)
    })
  }

  def write(writer: PrintWriter, d: Document): Unit = {
    val entries = entityEntries(d)
    for(e <- entries) {
      writer.println("%s\t%s\t%s\t%d" format(d.id, e.id, e.toks.map(_.trim).mkString("|"), e.date.getTime))
    }
  }
}

object StalenessFileWriter {
  def main(args: Array[String]): Unit = {
    def baseDir = "/Users/sameer/Google Drive/UW/D2D/D2D Nov 14/" //"/home/sameer/data/d2d/demo2015/nov/"
    def inputFile = baseDir + "nigeria_dataset_v04.nlp.json.gz"
    def outputFile = baseDir + "nigeria_dataset_v04.staleness.txt"
    val reader = new PerLineJsonReader()
    val docs = reader.read(inputFile)
    val writer = new StalenessFileWriter()
    writer.write(outputFile, docs)
  }
}
