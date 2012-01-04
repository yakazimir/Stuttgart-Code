import GRAM.Grammar
import scala.util.control.Breaks._
import scala.collection.mutable.Set

object CYK {

  case class Entry(pos:Product, value:Set[String])

  val gram = new Grammar
  gram.init

  class Parser (gram:Grammar,in:Input, ta:Chart){
    val input = in.pos 
    val table = ta.x

    def Parse {
      for (i <- input) {
	if ((gram.tList).contains(i)){ 
	  println("yes")
	}     
	else {println("not in grammar")}
      }     
    }
  }

  class Input(x:String) { 
    val pos : List[String] = (x.split(" ")).toList
    val original = println("input: "+x)
  }

  class Chart { 
    val x:Map[Product,Set[String]] = Map()
    

  } 
  
 
  def main(args: Array[String]){ 
    println("============\n\n============")
    var x:String = ""
    while (x != "Q"){ 
      print("PARSE> "); var x = readLine()
      var chart = new Chart
      var input = new Input(x.toLowerCase)
      var parser = new Parser(gram, input, chart)
      parser.Parse
      
    }    
  }

}



//println(input)
    //println(gram.tList)
