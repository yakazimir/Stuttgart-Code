import scala.xml._
import scala.io.Source
import scala.xml.pull.{XMLEventReader,EvElemStart, EvElemEnd, EvText,XMLEvent}

object reader {
  
  abstract class Rule 
  case class l(r:((Int,String,Int),String,Double)) extends Rule 
  case class b(r:((Int,String,Int),((Int,String,Int),(Int,String,Int)),Double)) extends Rule
  case class prule(r:((Int,String,Int),Double)) extends Rule 
  case class lex(r:(Int,String,Int)) extends Rule 


  var readingText = false 

  val src = Source.fromFile("forest2.xml")
  val er = new XMLEventReader(src)


  private def getAttrVals(x : String, attributes:xml.MetaData) : Rule = { 

    var st = attributes.apply("start").toString.toInt
    var sp = attributes.apply("span").toString.toInt
    var cat = attributes.apply("cat").toString
        
    x match { 
      case "Binary"|"Unary" => { 
	var weight = attributes.apply("weight").toString.toDouble
	return prule(((st,cat,sp),weight))
      }
      case _ => 
	var word = attributes.apply("word").toString
	return lex((st,word,sp))
    }
  }

  private def sentenceEvent(ev : XMLEventReader) {
    def toFormat(ty : String, sev : XMLEventReader, h : Rule) { 
      var done2 = false
      while (sev.hasNext && !done2){
	if (ty == "Binary"){ 
	  sev.next match {
	    case EvElemStart(_,"C1",atrs,_) => { 
	      //getAttrVals("C1",atrs)
	    }
	    case EvElemStart(_,"C2",atrs,_) => {
	      //println(getAttrVals("C2",atrs))
	    }
	    case EvElemEnd(_,"Binary") => {
	      println("binary end")
	      done2 = true 
	    } 
	    case _ => 
	  }
	}
	else {
	  sev.next match {
	    case EvElemStart(_,"terminal",atrs,_) => {
	      //val first = getAttrVals("terminal",atrs)
	    }
	    case EvElemEnd(_,"Unary") => {
	      done2 = true 
	      println("unary end")
	    }
	    case _ => 
	  }
	
	}
      }
    }
    var done = false 
    while (ev.hasNext && !done) {
      ev.next match {
	case EvElemStart(_,"Binary",atr,_) => {
	  val headRule = getAttrVals("Binary",atr)
	  toFormat("Binary",ev, headRule)
	}
	case EvElemStart(_,"Unary",atr,_) => {
	  val headRule = getAttrVals("Unary",atr)
	  toFormat("Unary", ev, headRule)
	}
	case EvElemEnd(_,"Rules") => {
	    done = true 
	}
	case _ =>       
      }
    }
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

