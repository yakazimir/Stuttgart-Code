import CYK._
import GRAM.Grammar
import scala.io.Source
import scala.collection.mutable.{Set,Map}

object Parser { 

  val gram = new Grammar; gram.init
  val file = Source.fromFile("sentences.txt").getLines.toList

  def time(f: => Unit)= {
	val s = System.currentTimeMillis; f
	System.currentTimeMillis - s
  }

  class KnuthDijkstra(x: Chart, i : Input) { 
   
    private var MaxVal : Map[String,Double] = Map()
    val forest = x.forest
    val terminals = i.original.split(" ").toList

    
    
  
  }


 
  def main(args: Array[String]){
    for (sentence <- file) { 
      
      val chart = new Chart
      var input = new Input(sentence.toLowerCase) 
      var parser = new Parser(gram, input, chart) 
      var timeFun : Double = time(parser.Parse)

      print("INPUT: "); input.inWSpans
      println("CPU Time: " + timeFun / 1000.0) 
      
   
      chart.printForest
      var knuth = new KnuthDijkstra(chart, input) 
   

    }
  
  }

}
