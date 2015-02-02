package nlp_serde.apps.d2d

import java.io.{PrintWriter, File}

import nlp_serde.Document
import nlp_serde.readers.PerLineJsonReader
import nlp_serde.writers.PerLineJsonWriter

import scala.util.Random

/**
 * @author sameer
 * @since 1/24/15.
 */
object RelevantFilter {
  
  def filter(d: Document, searchType: String): Boolean = {
    val date = d.attrs("date")
    val title = d.attrs("title")
    val path = d.path.get
    searchType match {
      case "chinese-amb" => {
        val dis = Set("allafrica.com/Jul2013-Nov2014/txt/Nigeria/militant groups/Boko Haram/stories/201405260987.txt",
          "www.ngrguardiannews.com/txt/national-news/news/national-news/181402-nigeria-china-celebrate-independence-with-prospect-of-better-relations.txt",
          "nigeria-dataset/from-randy/SpeechattheReceptionontheOccasionofAssumingDutybyChineseAmbassador.txt")
        dis.exists(suff => path.endsWith(suff))
      }
      case "threat" => {
        val dis = Set("nigeria-dataset/from-randy/TheriseofBokoHaram.txt",
          "nigeria-dataset/from-randy/WhyNigeriaWasAbletoBeatEbolabutNotBokoHaram.txt",
          "allafrica.com/Jul2013-Nov2014/txt/Nigeria/militant groups/Boko Haram/stories/2014050510543d58.txt",
          "www.pmnewsnigeria.com/Jul2013-Nov2014/txt/2014/11/07/more-troops-needed-to-stem-boko-haram-says-ngilari/index.txt",
          "www.tribune.com.ng/txt/news/top-stories/item/23167-self-defence-is-jihad-against-boko-haram-ibb.txt")
        dis.exists(suff => path.endsWith(suff))
      }
      case "kabiru" => {
        val dis = Set("allafrica.com/Jul2013-Nov2014/txt/Nigeria/militant groups/Boko Haram/stories/201307110345.txt",
          "allafrica.com/Jul2013-Nov2014/txt/Nigeria/militant groups/Ansaru/stories/201408042778.txt",
          "www.pmnewsnigeria.com/Jul2013-Nov2014/txt/2013/12/20/kabiru-sokoto-christmas-day-bomber-jailed-for-life/index.txt")
        dis.exists(suff => path.endsWith(suff))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val input = args(0)
    val file = new File(input)
    val dir = file.getParent
    val name = file.getName
    for(searchType <- Seq("threat", "chinese-amb", "kabiru")) {
      val output = dir + "/" + searchType + "." + name
      println(s"Reading from $input into $output")
      val writer = new PerLineJsonWriter(true)
      val reader = new PerLineJsonReader(true)
      writer.write(output, reader.read(input).filter(d => {
        filter(d, searchType)
      }))
    }
  }

}
