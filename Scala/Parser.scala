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

    def printMostProbable(){
      var string1 = "%-2s => %-8s %-8s"; var string2 = "%-2s => %-8s"
      println("Most Probable: "+log10(maxVal((0,"S",(i.pos).length))._2))

      def iterPrint(start:(Int,String,Int)){
	var i = maxVal(start)._1
	i match { 
	  case x : riVal => println("\t"+string1.format(start._2,x.s._1,x.s._2))
	    iterPrint(x.s._1); iterPrint(x.s._2) 
	  case y : rlVal => 
	    println("\t"+string2.format(start._2,y.b._2));
	    iterPrint(y.b)
	  case z : rsVal => None
	    //println(z.b)
	  
	}
      }
      print("\n")
      iterPrint((0,"S",(i.pos).length))
      print("\n")
    
    }

    def mostProbable(){
      for (part <- x.forest.reverse) { 
	part match { 
	  case x : lexical => 
	    maxVal += (x.l._1 -> (rsVal(x.l._2),x.l._3))
	  case y : singl => 
	    maxVal += ((y.b._1) -> (rlVal(y.b._2),y.b._3))
	  case z : binary => 
	    var prob : Double = maxVal(z.bi._2._1)._2 * maxVal(z.bi._2._2)._2;
	    try {
	      if (!maxVal.contains(z.bi._1) && maxVal(z.bi._1)._2 > prob){
		maxVal += (z.bi._1 -> (riVal((z.bi._2._1,z.bi._2._2)),prob * z.bi._3))}} 
	    catch { 
	      case eof: java.util.NoSuchElementException => 
		maxVal += (z.bi._1 -> (riVal((z.bi._2._1,z.bi._2._2)),prob * z.bi._3))}
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
       var timeMostProb : Double = time(knuth.mostProbable())
   

      print("INPUT: "); input.inWSpans
      println("CPU Time: " + (timeFun+timeMostProb) / 1000.0) 
      
      knuth.printMostProbable

    }
  
  }

}
