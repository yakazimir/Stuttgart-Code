import GRAM.Grammar
import scala.io.Source
import scala.collection.mutable.{Set,Map, HashSet}
import java.util.Formatter._

object CYK {  

  abstract class chartVals
  case class posP(numPair:(Int,Int)) extends chartVals
  
  val file = Source.fromFile("sentences.txt").getLines.toList
  val gram = new Grammar; gram.init
  
  def time(f: => Unit)= {
	val s = System.currentTimeMillis; f
	System.currentTimeMillis - s
  }

  def makeP(acc:Set[String],se:Set[String]) = 
    for (a <- acc; s <- se) yield {(a,s)}
 
  class Parser(gram:Grammar, in : Input, table : Chart){
    
    val input = in.pos

    private def allSpan(k : Int, z : Int, j : Int){
      try {
	var x = table.position((k,z)); var y = table.position((z,j)) 
	def prod(pair : Set[String]) = { 
	  var ter : Set[String] = Set() 
	  for (i <- pair) ter += i; ter
	}
	for(pair <- makeP(prod(x),prod(y)))
	  table.Add[(String,String)](posP((k,j)),pair,z,gram.rReverseMap)
      }
      catch { 
	case eof: java.util.NoSuchElementException => 
	  None
      }
    }

    def Parse(){
      def inParse(i : Int, j : Int){
	var k : Int = j - 2 
	while (k > -1){
	  var z : Int = k+1 
	  while (z < j){
	    allSpan(k,z,j);z+=1}
	  k -= 1
	}
      }
      for (word <- input){ 
	if ((gram.terminals).contains(word._1)){
	  var i : Int = word._2; var j : Int = i+1 
	  table.Add[String](posP((i,j)),word._1,0,gram.tReverseMap)
	  inParse(i,j)
	}
	else {println("not in grammar")}
      }
    }
  
  }

  class Chart {

    val position : Map[Product,Set[String]] = Map() 
    var forest : List[Product] = List() 

    abstract class forestForm 
    case class lexical(l:((Int,String,Int),String,Double)) extends forestForm
    case class binary(b:((Int,String,Int),(String,String),Double)) extends forestForm 

    def printForest { 
      
    }
    

    def Add[T](pos:posP,in:T,mid :Int,gram:Map[T,Set[(String,Double)]]){ 
      def pForest(entry : Set[(String,Double)]){
	var i = pos.numPair._1; var j = pos.numPair._2
	if (mid == 0){ 
	  forest = lexical(((i,in,j),in,1.0))::forest
	}
	else {println("") }
      }
      gram.get(in) match { 
	case Some(entry) => 
	  if (!position.contains(pos)){
	    position += (pos.numPair -> entry.unzip._1)
	    //pForest(entry)
	  }
	  else {
	    position(pos.numPair) ++ entry.unzip._1
	  }
	case None => None 
      }
      
    }

  }

  class Input(x : String) { 

    val pos = ((x.split(" ")).toList).zipWithIndex
    val original = x

    def inWSpans {
      for (word <- pos) {
	var f = word._2; print(f+" "+word._1+" ")
      }
      print(pos.length+"\n")
    } 
  }


  def main(args: Array[String]){
    for (sentence <- file) { 
      
      val chart = new Chart
      var input = new Input(sentence.toLowerCase) 
      
      var parser = new Parser(gram, input, chart)
      parser.Parse()
      println(chart.position)
      //var knuth = new KnuthDijkstra(chart, input)
      //var timeFun : Double = time(parser.Parse())
      //var timeMostProb : Double = time(knuth.mostProbable())

      //print("INPUT: "); input.inWSpans
      //println("CPU Time: " + (timeFun) / 1000.0) 


    }
  
  }


}  

 
