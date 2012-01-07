import GRAM.Grammar
import scala.util.control.Breaks._
import scala.collection.mutable.{Set,Map}
//import java.util.Formatter._
//import java.lang.Runtime._

object CYK {

  case class Entry(pos:Product, value:Set[String])

  def makeP(acc:Set[Any],se:Set[Any]) = 
    for (a <- acc; s <- se) yield {(a,s)}

  def time(f: => Unit)= {
	val s = System.currentTimeMillis; f
	System.currentTimeMillis - s
  }
 
  val gram = new Grammar
  gram.init
  
  class Parser (gram:Grammar,in:Input, table:Chart){
    
    val input = in.pos 
    
    private def allSpan(k : Int, z : Int, j : Int){
      try {
	var x = table.x((k,z)); var y = table.x((z,j)) 
	def prod(pair : Set[Product]) = { 
	  var ter : Set[Any] = Set() 
	  for (i <- pair) ter += i.productElement(0);ter
	}
	table.pAdd((k,j),makeP(prod(x),prod(y)))
      }
      catch { 
	case eof: java.util.NoSuchElementException => 
	  None
      }
    }

    private def inParse(i:Int,j:Int) { 
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
	  table.lAdd((i,j),word._1); inParse(i,j)
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

    val x:Map[Product,Set[Product]] = Map()   
    
    //section function should be merged with first

    def lAdd(pos: Product, in : String) {
      (gram.revMap).get(in) match { 
	case Some(entry) 
	  => x += (pos -> entry)
	case None 
	  => println("an error occurred");break
      }
    }

    def pAdd(pos: Product, in : Set[(Any,Any)]) { 
      for (item <- in) { 
	(gram.revMap).get(item) match { 
	  case Some(entry) 
	    => x += (pos -> entry)
	  case None 
	    => None
	}
      }    
    }

    def printChart(in : Input)  = { 
      print("input: "); in.inWSpans

      for (i <- 0 to ((in.pos).length)-1) { 
	print(i)
	for (j <- 1 to ((in.pos).length)){ 

	  x.get((i,j)) match{ 
	    case Some(item) => print(" "+item+" "+j)
	    case None => print(" "+"Nil"+" "+j)
	  }
	}
	print("\n") 
      }
      print("\n")
    }

  } 

    
  def main(args: Array[String]){ 
    println("============\n\n============")
    var x:String = ""
    while (x != "Q"){ 
      print("PARSE> "); var x = readLine()
      var chart = new Chart; var input = new Input(x.toLowerCase)
      var parser = new Parser(gram, input, chart)
      var timeFun : Double = time(parser.Parse)
      println("CPU Time: " + timeFun / 1000.0); chart.printChart(input)
      
    }    
  }

}



