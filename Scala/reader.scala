import scala.xml._
import scala.io.Source
import scala.xml.pull.{XMLEventReader,EvElemStart, EvElemEnd, EvText,XMLEvent}

object reader {
  
  abstract class Rule { 
    def head:(Int,String,Int)= (0," ",0);
    def dep:Any={ };def ruleType:Any={ };def prob:Double=0.0   
  }
  case class l(r:((Int,String,Int),(Int,String,Int),Double)) extends Rule { 
    override def head : (Int,String,Int) = {r._1}
    override def dep : String = {r._2._2}
    override def ruleType : (String,String) = {(r._1._2,r._2._2)}
  }
  case class b(r:((Int,String,Int),((Int,String,Int),(Int,String,Int)),Double)) extends Rule {
    override def head : (Int,String,Int) = {r._1}
    override def dep : ((Int,String,Int),(Int,String,Int)) = {r._2}
    override def ruleType : (String,String,String) = {(r._1._2,r._2._1._2,r._2._2._2)}    
  }
  case class prule(r:((Int,String,Int),Double)) extends Rule { 
    override def head : (Int,String,Int) = r._1
    override def prob : Double = r._2
  } 
  case class lex(r:(Int,String,Int)) extends Rule {
    override def head : (Int,String,Int) = {r}
  }

  var readingText = false 

  val src = Source.fromFile("forest2.xml")
  val er = new XMLEventReader(src)

  private def getAttrVals(x : String, attributes:xml.MetaData) : Rule = { 

    var st = attributes.apply("start").toString.toInt
    var sp = attributes.apply("span").toString.toInt
        
    x match { 
      case "Binary"|"Unary" => { 
	var cat = attributes.apply("cat").toString 
	var weight = attributes.apply("weight").toString.toDouble
	return prule(((st,cat,sp),weight))
      }
      case "C1"|"C2" => {
	var cat = attributes.apply("cat").toString
	return lex((st,cat,sp))
      }
      case "terminal" => {
	var word = attributes.apply("word").toString
	return lex((st,word,sp))
      }
    } 
  }

  private def sentenceEvent(ev : XMLEventReader) : List[Rule] = {
    def toFormat(des : String, sev : XMLEventReader): Rule =   { 
      var done2 = false
      var result : Rule = lex((0,"f",0))
      while (sev.hasNext && !done2){
	  sev.next match {
	    case EvElemStart(_,des,atrs,_) => {
	      result = getAttrVals(des,atrs)
	      done2 = true 
	    }
	    case _ => 
	  }
      }
      return result 
    }
    var done = false 
    var forest : List[Rule] = List()
    while (ev.hasNext && !done) {
      ev.next match {
	case EvElemStart(_,"Binary",atr,_) => {
	  val headRule = getAttrVals("Binary",atr)
	  var r1 = toFormat("C1",ev).head; val r2 = toFormat("C2",ev).head
	  forest = b((headRule.head,(r1,r2),headRule.prob))::forest
	}
	case EvElemStart(_,"Unary",atr,_) => {
	  val headRule = getAttrVals("Unary",atr)
	  var terminal = toFormat("Unary", ev).head
	  forest = l((headRule.head,terminal,headRule.prob))::forest
	}
	case EvElemEnd(_,"Rules") => {
	    done = true 
	}
	case _ =>       
      }
    }
    return forest 
  }
 
  def main(args: Array[String]) {

    val src = Source.fromFile("forest2.xml")
    val er = new XMLEventReader(src)
  
    while (er.hasNext){
      er.next match {
	case EvElemStart(_,"sentence",attr,_) => 
	  sentenceEvent(er)
	case EvElemEnd(_,"sentence") =>
	  println("sentence ended") 
	case _ => 
      }
    }
    
  }
}

