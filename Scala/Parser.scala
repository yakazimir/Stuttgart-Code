import CYK._
import GRAM.Grammar
import scala.io.Source
import scala.collection.mutable.{Set,Map}
import java.util.Formatter._
import scala.math._
import mostProbable._

object Parser { 
  
  def time(f: => Unit)= {
    val s = System.currentTimeMillis; f
    System.currentTimeMillis - s
    }

  val gram = new Grammar; gram.init
  val file = Source.fromFile("sentences.txt").getLines.toList

  def main(args: Array[String]){
    for (sentence <- file) { 
      
      val chart = new Chart
      var input = new Input(sentence.toLowerCase) 
      var parser = new Parser(gram, input, chart) 
      var knuth = new KnuthDijkstra(chart, input)
      
      var timeFun : Double = time(parser.Parse())
      var timeMostProb : Double = time(knuth.mostProbable())
   

      print("INPUT: "); input.inWSpans
      println("CPU Time: " + (timeFun+timeMostProb) / 1000.0) 
      
      chart.printForest
      println("=======================")
      knuth.printMostProbable

    }
  
  }

}
