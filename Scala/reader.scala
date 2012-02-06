import scala.xml._
import scala.io.Source
import scala.xml.pull.{XMLEventReader,EvElemStart, EvElemEnd, EvText,XMLEvent}
import scala.collection.mutable.{Set,Map, HashSet}

object reader {
  
  abstract class Rule { 
    def head:(Int,String,Int)= (0," ",0); def headLoc:(Int,Int)={(0,0)}
    def prob:Double=0.0   
  }
  case class l(r:((Int,String,Int),(Int,String,Int),Double)) extends Rule { 
    override def head : (Int,String,Int) = {r._1}
    override def headLoc:(Int,Int) = {(r._1._1,r._1._3)} 
    override def prob : Double = {r._3}
    def dep : (Int,String,Int) = {r._2}; 
    def ruleType : (String,String) = {(r._1._2,r._2._2)}
  }
  case class b(r:((Int,String,Int),((Int,String,Int),(Int,String,Int)),Double)) extends Rule {
    override def head : (Int,String,Int) = {r._1}; 
    override def headLoc:(Int,Int) = {(r._1._1,r._1._3)} 
    override def prob : Double = {r._3}
    def ruleType : (String,String,String) = {(r._1._2,r._2._1._2,r._2._2._2)} 
    def dep : ((Int,String,Int),(Int,String,Int)) = {r._2}
    def dep1 : (Int,String,Int) = dep._1
    def dep2 : (Int,String,Int) = dep._2 
  }
  case class prule(r:((Int,String,Int),Double)) extends Rule { 
    override def head : (Int,String,Int) = r._1
    override def prob : Double = r._2
  } 
  case class lex(r:(Int,String,Int)) extends Rule {
    override def head : (Int,String,Int) = {r}
  }
  case class rule1(r:((String,String,String),Double)) extends Rule  
  case class rule2(r:((String,String),Double)) extends Rule 
  case class lo(r:(Int,Int)) extends Rule 
  case class lo2(r:((Int,Int),(Int,Int),(Int,Int))) extends Rule 

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
	case EvElemEnd(_,"sentence") => {
	    done = true 
	}
	case _ =>       
      }
    }
    return forest 
  }

  class insideOutProb(x : List[Rule], size: Int) { 
    
    var inside : Map[(Int,Int),Map[String,Double]] = Map() 
    var outside : Map[(Int,Int),Map[String,Double]] = Map() 
    var rules : Map[Rule,List[Rule]] = Map() 

    private def checkExist(x:(Int,Int),y:String,y2:Double,z:Map[(Int,Int),Map[String,Double]]){   
      if (!z.contains(x)){
	z += (x -> Map(y -> y2))
      }
    else {
      if (z(x).contains(y)){
	var upd : Double = z(x)(y) + y2
	z(x) += (y -> upd)
      } 
      else {z(x) += (y -> y2)}
    }
   }
    
    private def insideProb():Map[(Int,Int),Map[String,Double]] = {   
      for (item <- x){ 
	item match { 
	  case x : l => {
	    checkExist(x.headLoc,x.head._2, x.prob,inside)
	}
	  case y : b => { 
	    var f : Double = (y.prob * inside((y.dep1._1,y.dep1._3))(y.dep1._2)*
			      inside((y.dep2._1,y.dep2._3))(y.dep2._2))
	      checkExist(y.headLoc,y.head._2,f,inside)
	  }
	  case _ => 	
	}
      }
      return inside 
    }

    private def outsideProb() = { 
      def addRuleType(r:Rule,pos:Rule){
	if (!rules.contains(r)){
	  rules += (r -> List(pos))
	}
	else {rules(r) = pos::rules(r)}	
      } 
      def rec(fir:(Int,String,Int),sec:((Int,String,Int),(Int,String,Int)),prob:Double) {
	checkExist(
	  (sec._1._1,sec._1._3),sec._1._2,prob*(inside(sec._2._1,sec._2._3)(sec._2._2))*
	  outside(fir._1,fir._3)(fir._2),outside) 	
	checkExist(
	  (sec._2._1,sec._2._3),sec._2._2,prob*(inside(sec._1._1,sec._1._3)(sec._1._2))*
	  outside(fir._1,fir._3)(fir._2),outside)	
      }
      for (item <- x.reverse) { 
	item match { 
	  case x : l => 
	    addRuleType(rule2((x.ruleType,x.prob)),lo(x.headLoc))
	  case y : b => 
	    addRuleType(rule1((y.ruleType, y.prob)),
			      lo2((y.headLoc,(y.dep1._1,y.dep1._3),(y.dep2._1,y.dep2._3))))
	    if (y.head == (0,"S",size)){
	      checkExist(y.headLoc,y.head._2,1.0,outside)
	      rec(y.head,y.dep,y.prob)
	    }
	    else { 
	      rec(y.head,y.dep,y.prob)
	    }
	  case _ => 
	}	
      }
    }

    var insideVals = insideProb() 
    var outsideVals = outsideProb()

    //for (item<-insideVals)println(item)
    println("========")
    for (item<-outside)println(item)

   
  }

  def main(args: Array[String]) {

    val src = Source.fromFile("forest2.xml")
    val er = new XMLEventReader(src)
    var inputSize : Int = 0
  
    while (er.hasNext){
      er.next match {
	case EvElemStart(_,"Length",attr,_) => 
	  inputSize = attr.apply("lVal").toString.toInt
	case EvElemStart(_,"Rules",_,_) => 
	  var forest = sentenceEvent(er)
	  var insideO = new insideOutProb(forest, inputSize)
	case EvElemEnd(_,"trainingData") =>
	  println("end training") 
	case _ => 
      }
    }
    
  }
}

