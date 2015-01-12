package nlp_serde.apps.d2d

import java.io.{PrintWriter, File}
import java.text.SimpleDateFormat

import nlp_serde.Document
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.{PerLineJsonWriter, Writer, DocPerFile}

/**
 * Created by sameer on 11/6/14.
 */
class SummaFormatWriter(original: Boolean = true,
                        corenlp: Boolean = true,
                        extraction: Boolean = true) extends Writer {
  override def write(baseDir: String, docs: Iterator[Document]): Unit = {
    if(original) {
      // original
      new File(baseDir + "/original/").mkdirs()
      val fileList = new PrintWriter(baseDir + "/filelist.txt")
      for (d <- docs.filter(_.attrs.contains("date"))) {
        val name = baseDir + "/original/" + d.id + ".story.xml"
        println(name)
        fileList.println(name)
        val writer = new PrintWriter(name)
        writer.println("<DOC>")
        writer.print("<DATETIME>")
        val parse = new SimpleDateFormat("yyyy-MM-dd")
        val date = parse.parse(d.attrs("date"))
        val format = new SimpleDateFormat("yyyy-M-dd HH:mm:ss")
        writer.print(format.format(date))
        writer.println("</DATETIME>")

        writer.print("<TITLE>")
        writer.print(d.attrs.get("title").getOrElse("No Title"))
        writer.println("</TITLE>")

        writer.println("<TEXT>")
        for(s <- d.sentences) {
          writer.println(s.text)
          writer.println()
        }
        writer.println("</TEXT>")
        writer.println("</DOC>")
        writer.flush()
        writer.close()
      }
      fileList.flush()
      fileList.close()
    }
    if(corenlp) {
      // corenlp
      // annotators.txt looks like:
      // annotators = tokenize, cleanxml, ssplit, pos, lemma, ner, parse, dcoref
      // ssplit.newlineIsSentenceBreak = two
      val annotators = new PrintWriter(baseDir + "/annotators.txt")
      annotators.println("annotators = tokenize, cleanxml, ssplit, pos, lemma, ner, parse, dcoref")
      annotators.println("ssplit.newlineIsSentenceBreak = two")
      annotators.flush()
      annotators.close()
      // run corenlp
      // java -cp "*" -Xmx3g edu.stanford.nlp.pipeline.StanfordCoreNLP -props annotators.txt -filelist <FILE WITH LIST OF FILENAMES> -outputDirectory <OUTDIRECTORY>
      edu.stanford.nlp.pipeline.StanfordCoreNLP.main(Array(
        "-props", baseDir + "/annotators.txt",
        "-filelist", baseDir + "/filelist.txt",
        "-outputDirectory", baseDir + "/corenlp/"
      ))
    }
    // empty dir
    new File(baseDir + "/extractions/").mkdirs()
  }
}

object FilterDocuments {
  def filter(d: Document, keyword: String, datePrefixes: Set[String]): Boolean = {
    //val datePrefixes = Set("2014-04","2014-05","2014-06")
    //val keyword = "kidnap"
    val date = d.attrs("date")
    val title = d.attrs("title")
    (datePrefixes.isEmpty || datePrefixes.exists(s => date.startsWith(s))) && title.toLowerCase.contains(keyword)
  }

  def main(args: Array[String]): Unit = {
    val input = args(0)
    val keyword = args(1).toLowerCase
    val datePrefixes = args.drop(2).toSet
    val file = new File(input)
    val dir = file.getParent
    val name = file.getName
    val output = dir + "/" + keyword + "." + name
    println(s"Reading from $input into $output")
    val writer = new PerLineJsonWriter(true)
    val reader = new PerLineJsonReader(true)
    writer.write(output, reader.read(input).filter(d => filter(d, keyword, datePrefixes)))
  }

}

object SummaFormatWriter {
  def main(args: Array[String]): Unit = {
    val name = if(args.isEmpty) "kidnap" else args(0)
    def inputFile = "data/d2d/"+name+".docs.nlp.flr.json.gz"
    def outputDir = "data/d2d/summa/" + name
    val reader = new PerLineJsonReader()
    val docs = reader.read(inputFile)
    val writer = new SummaFormatWriter()
    writer.write(outputDir, docs)
  }
}
