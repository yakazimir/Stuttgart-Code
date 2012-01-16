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
    var forest : List[forestForm] = List() 

    abstract class forestForm 
    case class lexical(l:((Int,String,Int),String,Double)) extends forestForm
    case class singl(b:((Int,String,Int),(Int,String,Int),Double)) extends forestForm 
    case class binary(bi:((Int,String,Int),((Int,String,Int),(Int,String,Int)),Double)) extends forestForm

    def printForest = { 
      val string = "%-15s ==> %-20s %-5s"; print("\n")
      for (item <- forest){ 
	item match { 
	  case x : lexical => println("\t"+string.format(x.l._1,x.l._2,x.l._3))
	  case y : singl => println("\t"+string.format(y.b._1, y.b._2, y.b._3)) 
	  case z : binary => println("\t"+string.format(z.bi._1, z.bi._2, z.bi._3))
	}
      }   
    }
    
    def Add[T](pos:posP,in:T,mid :Int,gram:Map[T,Set[(String,Double)]]){ 
    
      def tableA(pos : posP, entry : Set[(String,Double)]){
	if (!position.contains(pos)){
	  position += (pos.numPair -> entry.unzip._1)
	}
	else {position(pos.numPair)++ entry.unzip._1}
      }
      gram.get(in) match { 
	case Some(entry) => tableA(pos,entry);var i = pos.numPair._1;var j = pos.numPair._2
	if (mid == 0){var inS = in.asInstanceOf[String]
	    forest = lexical(((i,inS,j),inS,1.0))::forest
	    for (value <- entry) { 
	      forest = singl(((i,value._1,j),(i,inS,j),value._2))::forest }
	  }
	else { var ino = in.asInstanceOf[(String,String)]
	  for (value <- entry) { 
	    forest = binary(((i,value._1,j),((i,ino._1,mid),(mid,ino._2,j)),value._2))::forest }
	}
	case None => 
	  None 
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
      chart.printForest
      //var knuth = new KnuthDijkstra(chart, input)
      //var timeFun : Double = time(parser.Parse())
      //var timeMostProb : Double = time(knuth.mostProbable())

      //print("INPUT: "); input.inWSpans
      //println("CPU Time: " + (timeFun) / 1000.0) 


    }
  
  }


}  

 
