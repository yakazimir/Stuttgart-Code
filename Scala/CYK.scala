import GRAM.Grammar
import scala.util.control.Breaks._
import scala.collection.mutable.Set

object CYK {

  case class Entry(pos:Product, value:Set[String])

  val gram = new Grammar
  gram.init

  class Parser (gram:Grammar,in:Input, table:Chart){
    val input = in.pos 

    def Parse {
      for (word <- input) {
	if ((gram.tList).contains(word)){ 
	  var i = input.indexOf(word); var j=i+1
	  table.Add((i,j),word)
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
    
    def Add(pos: Product, in : String) {
      (gram.revMap).get(in) match { 
	case Some(entry) => println(entry);println(pos)
	case None => println("an error occurred")
      }

    }
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
