import GRAM.Grammar
import scala.util.control.Breaks._
import scala.collection.mutable.{Set,Map}
import scala.io.Source

//import java.util.Formatter._
//import java.lang.Runtime._

object CYK {
  
  val gram = new Grammar; gram.init
  val file = Source.fromFile("sentences.txt").getLines.toList
   
  def makeP(acc:Set[String],se:Set[String]) = 
    for (a <- acc; s <- se) yield {(a,s)}

  def time(f: => Unit)= {
	val s = System.currentTimeMillis; f
	System.currentTimeMillis - s
  }
 
  class Parser (gram:Grammar,in:Input, table:Chart){
    
    val input = in.pos 
    
    private def allSpan(k : Int, z : Int, j : Int){
      try {
	var x = table.position((k,z)); var y = table.position((z,j)) 
	def prod(pair : Set[String]) = { 
	  var ter : Set[String] = Set() 
	  for (i <- pair) ter += i;ter
	}
	for(pair <- makeP(prod(x),prod(y))) table.Add((k,j),pair)
      }
      catch { 
	case eof: java.util.NoSuchElementException => 
	  None
      }
    }

    private def inParse(i : Int,j : Int) { 
      var k : Int  = j - 2 
      while (k > - 1){
	var z : Int = k+1
	while (z < j) {
	  allSpan(k,z,j); z += 1	
	}
	k -= 1
      }
    }
    
    def Parse {
      for (word <- input) {
	if ((gram.tList).contains(word._1)){ 
	  var i = word._2; var j = i+1
	  table.Add((i,j),word._1); inParse(i,j)
	}     
	else {println("not in grammar")} 
      }     
    }
  }

  class Input(x : String) { 

    val pos = ((x.split(" ")).toList).zipWithIndex
    val original = "input: "+ x

    def inWSpans {
      for (word <- pos) {
	var f = word._2; print(f+" "+word._1+" ")
      }
      print(pos.length+"\n"); print("\n")
    } 
  }

  class Chart { 

    val position : Map[Product,Set[String]] = Map()
    var parseForest : List[Product] = List()

    def Add(pos: Product, in : Any) {
      
      def S(entry : Set[Product]) = {
	var tableSet : Set[String] = Set() 
	for (piece <- entry){ 
	  tableSet += (
	    piece.productElement(0)).asInstanceOf[String]
	}
	tableSet
      }

      (gram.revMap).get(in) match { 
	case Some(entry) => 
	  if (position.contains(pos)){
	      position(pos) ++ S(entry)
	  }
	  else {position += (pos -> S(entry))}

	case None => 
	  None
      }
    }

    private def writeParseForest {
      
      
      // THIS IS WHERE 

    }

    //print real chart when figured out java.formatter
    def printChart(in : Input)  = { 
      print("input: "); in.inWSpans

      for (i <- 0 to ((in.pos).length)-1) { 
	print(i)
	for (j <- 1 to ((in.pos).length)){ 

	  position.get((i,j)) match{ 
	    case Some(item) => print(" "+item.mkString(",")+" "+j)
	    case None => print(" "+" "+" "+j)
	  }
	}
	print("\n") 
      }
      print("\n")
    }

  } 

  def main(args: Array[String]){ 
    for (sentence <- file) { 
      var chart = new Chart; var input = new Input(sentence.toLowerCase) 
      var parser = new Parser(gram,input, chart) 
      var timeFun : Double = time(parser.Parse)
      
      println("CPU Time: " + timeFun / 1000.0); chart.printChart(input)

    }

  }   
}    
