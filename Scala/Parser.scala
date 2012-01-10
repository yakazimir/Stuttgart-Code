import CYK._
import GRAM.Grammar
import scala.io.Source
import scala.collection.mutable.{Set,Map}
import java.util.Formatter._

object Parser { 

  val gram = new Grammar; gram.init
  val file = Source.fromFile("sentences.txt").getLines.toList

  def time(f: => Unit)= {
	val s = System.currentTimeMillis; f
	System.currentTimeMillis - s
  }

  class KnuthDijkstra(x: Chart, i : Input) { 
   
    var maxVal : Map[Any,Double] = Map()
    //private var terminals = i.original.split(" ").toSet
    //private var forest = x.forest.reverse

    private def maxCheck (p : Any, pr: Any, x : Any, y : Any){
      var prob : Double = pr.asInstanceOf[Double] * maxVal(x) * maxVal(y) 
      
      try {
	if (!(maxVal.contains(p) && maxVal(p)<prob)){
	  maxVal += (p -> prob)
	}
      }
      catch { 
	case eof: java.util.NoSuchElementException => 
	  maxVal += (p -> prob)
      }
    }

    def mostProbable() {
       var terminals = i.original.split(" ").toSet
       var forest = x.forest.reverse
      
      for (part <- forest) { 
	try { 
	  if (terminals.contains(part.productElement(1).asInstanceOf[String])){
	    maxVal += (part.productElement(0) ->
		       part.productElement(2).asInstanceOf[Double])
	  }
	}     
	catch { 
	  case eof: java.lang.ClassCastException => 
	    if (maxVal.contains(part.productElement(1))){
	      var max : Double  = 
		part.productElement(2).asInstanceOf[Double] * maxVal(part.productElement(1))
	      maxVal += (part.productElement(0) -> max)
	      
	    }
	    else { 
	      val (x,y) = part.productElement(1)
	      maxCheck(part.productElement(0),part.productElement(2),x,y)      
	    }
	  
	}
      }
    }
    
    def printMostProbable {
      val string = "%-15s ==> %-20s";

      for (entry <- x.forest){ 
	//println(entry.productElement(0))
	if (maxVal.contains(entry.productElement(0))) { 
	  println("\t"+string.format(entry.productElement(0),maxVal(entry.productElement(0))))
	}


      }
    }


  }


  def main(args: Array[String]){
    for (sentence <- file) { 
      
      val chart = new Chart
      var input = new Input(sentence.toLowerCase) 
      var parser = new Parser(gram, input, chart) 
      var knuth = new KnuthDijkstra(chart, input)
      var timeFun : Double = time(parser.Parse)
      var timeMostProb : Double = time(knuth.mostProbable())

      print("INPUT: "); input.inWSpans
      println("CPU Time: " + (timeFun+timeMostProb) / 1000.0) 
      knuth.printMostProbable
      
      //var knuth = new KnuthDijkstra(chart, input) 
      //chart.printForest

    }
  
  }

}
