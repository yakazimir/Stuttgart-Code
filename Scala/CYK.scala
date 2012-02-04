import GRAM.Grammar
import scala.collection.mutable.{Set,Map, HashSet}
import java.util.Formatter._
import scala.xml._

object CYK {  

  abstract class chartVals
  case class posP(numPair:(Int,Int)) extends chartVals
  
  abstract class forestForm {
    def toXml():Any = { }
  }
  case class lexical(l:((Int,String,Int),String,Double)) extends forestForm 
  case class binary(bi:((Int,String,Int),((Int,String,Int),(Int,String,Int)),Double)) extends forestForm {
     override def toXml():xml.Elem = {
       <Binary cat={bi._1._2} start={bi._1._1.toString} span={bi._1._3.toString} weight={bi._3.toString}>{
       <C1 cat={bi._2._1._2} start={bi._2._1._1.toString} span={bi._2._1._3.toString}></C1>
       <C2 cat={bi._2._2._2} start={bi._2._2._1.toString} span={bi._2._2._3.toString}></C2>
       } 
       </Binary>
    }
  }
  case class singl(b:((Int,String,Int),(Int,String,Int),Double)) extends forestForm {
    override def toXml():xml.Elem = {
      <Unary cat={b._1._2} start={b._1._1.toString} span={b._1._3.toString} weight={b._3.toString}>
      {<terminal word={b._2._2} start={b._2._1.toString} span={b._2._3.toString}></terminal>}
      </Unary>
      
    }
  }

  def makeP(acc:Set[String],se:Set[String]) = for (a <- acc; s <- se) yield {(a,s)}
 
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

  class Chart(input:String, index:Int){

    val position : Map[Product,Set[String]] = Map() 
    var forest : List[forestForm] = List() 

    def printForest = { 
      val string = "%-15s ==> %-20s %-5s"; print("\n")
      for (item <- forest){ 
	item match { 
	  case x : lexical => 
	    println("\t"+string.format(x.l._1,x.l._2,x.l._3))
	  case y : singl => 
	    println("\t"+string.format(y.b._1, y.b._2, y.b._3)) 
	  case z : binary => 
	    println("\t"+string.format(z.bi._1, z.bi._2, z.bi._3))
	}
      }   
    }

    def forestToXML() = {
      val sentence = <sentence id={index.toString}>
      <Input>{input}</Input>
      <Length>{input.split(" ").length}</Length>
      <Rules>{for (i <- forest) yield i.toXml}</Rules>
      </sentence>

      val pp = new scala.xml.PrettyPrinter(100,2)
      println(pp.format(sentence))
      
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
	if (mid == 0){ var inS = in.asInstanceOf[String]
	    forest = lexical(((i,inS,j),inS,1.0))::forest
	    for (value <- entry) { 
	      forest = singl(((i,value._1,j),(i,inS,j),value._2))::forest }
	  }
	else { var ino = in.asInstanceOf[(String,String)]
	  for (value <- entry) { 
	    forest = binary(((i,value._1,j),((i,ino._1,mid),(mid,ino._2,j)),value._2))::forest }
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

}


 
