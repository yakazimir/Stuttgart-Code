import GRAM.Grammar
import scala.util.control.Breaks._
import scala.collection.mutable.{Set,Map}

object CYK {

  case class Entry(pos:Product, value:Set[String])

  val gram = new Grammar
  gram.init

  class Parser (gram:Grammar,in:Input, table:Chart){
    val input = in.pos 

    
    private def inParse(i:Int,j:Int) { 
      var k : Int  = j - 2 
      while (k > - 1){ 
	var z : Int = k+1; println(k) 
	while (z < j) { 
	  println("\t"+z)
	  z += 1
	}
	k -= 1
      }
    }
    
    def Parse {
      for (word <- input) {
	if ((gram.tList).contains(word)){ 
	  var i = input.indexOf(word); var j=i+1
	  table.Add((i,j),word)
	  inParse(i,j)
	}     
	else {println("not in grammar")} 
      }     
    }
  
  }

  class Input(x:String) { 

    val pos : List[String] = (x.split(" ")).toList
    //val original = println("input: "+x)

    def inWSpans {
      for (word <- pos) {
	var f = pos.indexOf(word) 
	print(f+" "+word+" ")
      }
      print(pos.length+"\n")
    }
    inWSpans

  }

  class Chart { 

    val x:Map[Product,Set[Product]] = Map()
    
    def Add(pos: Product, in : String) {
      (gram.revMap).get(in) match { 
	case Some(entry) => x += (pos -> entry)
	case None => println("an error occurred");break
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
