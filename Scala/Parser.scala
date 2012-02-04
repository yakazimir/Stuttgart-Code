import CYK._
import GRAM.Grammar
import scala.io.Source
import mostProbable._
import insideOut._

object Parser { 
  
  def time(f: => Unit)= {
    val s = System.currentTimeMillis; f
    System.currentTimeMillis - s
    }

  def toXml(entry : forestForm){
    
  }

  //println("Importing Grammar and Sentence file")
  val gram = new Grammar; var gramTime : Double = time(gram.init)
  val file = Source.fromFile("sentences.txt").getLines.toList
  var sentTime : Double = time(file)
  //println("CPU Time: "+(gramTime+sentTime)/ 1000.0+"\n")


  def main(args: Array[String]){
    for (sentence <- file) { 
      var input = new Input(sentence.toLowerCase) 
      var chart = new Chart(input.original, file.indexOf(sentence))
      var parser = new Parser(gram, input, chart) 
      //var knuth = new KnuthDijkstra(chart, input)
      
      var timeFun : Double = time(parser.Parse())
      
      chart.forestToXML()
      //var timeMostProb : Double = time(knuth.mostProbable())

      //var t = new insideProb(chart)

      //for (item<-chart.forest)println(item)
    }
  }
}

