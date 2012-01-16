import CYK._
import GRAM.Grammar
import scala.io.Source
import scala.collection.mutable.{Set,Map}
import java.util.Formatter._
import scala.math._

object Parser { 
  
  def time(f: => Unit)= {
    val s = System.currentTimeMillis; f
    System.currentTimeMillis - s
    }

  val gram = new Grammar; gram.init
  val file = Source.fromFile("sentences.txt").getLines.toList

  abstract class max 
  case class rlVal(b:(Int,String,Int)) extends max 
  case class rsVal(b:String) extends max
  case class riVal(s:((Int,String,Int),(Int,String,Int))) extends max 

  class KnuthDijkstra(x: Chart, i : Input) { 
   
    var maxVal : Map[(Int,String,Int),(max,Double)] = Map()

    def mostProbable(){
      var forest = x.forest.reverse
      for (part <- forest) { 
	part match { 
	  case x : lexical => 
	    maxVal += (x.l._1 -> (rsVal(x.l._2),x.l._3))
	  case y : singl => 
	    maxVal += ((y.b._1) -> (rlVal(y.b._2),y.b._3))
	  case z : binary => 
	    var prob : Double = maxVal(z.bi._2._1)._2 * maxVal(z.bi._2._2)._2;
	    try {
	      if (!maxVal.contains(z.bi._1) && maxVal(z.bi._1)._2 > prob){
		maxVal += (z.bi._1 -> (riVal((z.bi._2._1,z.bi._2._2)),prob * z.bi._3))
	      }
	    } 
	    catch { 
	      case eof: java.util.NoSuchElementException => 
		maxVal += (z.bi._1 -> (riVal((z.bi._2._1,z.bi._2._2)),prob * z.bi._3))
	    }
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
      
      var timeFun : Double = time(parser.Parse())
   

      print("INPUT: "); input.inWSpans
      println("CPU Time: " + (timeFun) / 1000.0) 
      
      knuth.mostProbable()
      //chart.printForest
      //println("==========================")
      //knuth.printMostProbable

    }
  
  }

}
