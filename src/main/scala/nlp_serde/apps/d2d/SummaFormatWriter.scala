package nlp_serde.apps.d2d

import java.io.{PrintWriter, File}
import java.text.SimpleDateFormat

import nlp_serde.Document
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.{Writer, DocPerFile}

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
        val name = baseDir + "/original/" + new File(d.path.get).getName.dropRight(4) + ".story.xml"
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
  }
}

object SummaFormatWriter {
  def main(args: Array[String]): Unit = {
    def inputFile = "/home/sameer/data/d2d/demo2015/nov/nigeria_dataset_v04.nlp.json.gz"
    def outputDir = "/home/sameer/data/d2d/demo2015/nov/summa/test"
    val reader = new PerLineJsonReader()
    val docs = reader.read(inputFile)
    val writer = new SummaFormatWriter()
    writer.write(outputDir, docs)
  }
}
