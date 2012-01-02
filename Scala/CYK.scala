
//import Grammar

object CYK {
  
  
  case class NT(lT:String, rT:String)
  
  class Parser { 
    var n : Int = _  
  }

  class Grammar (g:Map[String,NT]){ 
    println(g("S"))
  }
  
  class Input { 
  }


  val Gram = new Grammar(
    Map("S" -> NT("NP","VP"), 
	       //NT("NP","VP","PP"), 
	"NP" -> NT("N","V")))
 
  //var In = new Input

  def main(args: Array[String]){ 
    print("Input: ")
    var x = readLine()
    //print(Gram)
  }

}
