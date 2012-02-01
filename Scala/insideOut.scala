import CYK._
//import GRAM.Grammar
import scala.collection.mutable.{Set,Map, HashSet}

object insideOut { 


  abstract class Rule 
  case class B(r:((String,String,String),Double)) extends Rule
  case class PT(r:((String,String),Double)) extends Rule 
  case class Rule1(r:(String,String)) extends Rule
  case class Rule2(r:(String,String,String)) extends Rule
  case class ML(r:((Int,Int),(Int,Int),(Int,Int))) extends Rule
  case class L(r:(Int,Int)) extends Rule

  val count : Map[Rule,Double] = Map() 
  
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

  class insideOutProb(x:Chart,inputSize:Int){ 
    
    var inside : Map[(Int,Int),Map[String,Double]] = Map() 
    var outside : Map[(Int,Int),Map[String,Double]] = Map() 
    val rules : Map[Rule,List[Rule]] = Map() 

    val insideVals = new insideProb(x,inside)
    val outsideVals = new outsideProb(x,outside, inside, inputSize, rules)

    var sentenceProb : Double = inside(0,inputSize)("S")
    
    private def countCalc(){

        }

    for ((rule,pos) <- rules){ 
      def calcSum(ruleP:(String,String,String),rulePoss:List[Rule]):Double ={
	var product : Double = 0
	for (listE <- rulePoss){
 	  listE match { 
	    case lex : L => 
	      product +=  outside(lex.r)(ruleP._1)
	    case m : ML => {
	      try { product += (outside(m.r._1)(ruleP._1) * 
			  inside(m.r._2)(ruleP._2) * inside(m.r._3)(ruleP._3))}
	      catch  {case eof : java.util.NoSuchElementException => None}
	    }
	  }
	}
	product
      }
      rule match { 
	case x : PT =>  
	  println((x.r._2/sentenceProb)* calcSum((x.r._1._1,x.r._1._2," "),pos))
	case y : B => 
	  println((y.r._2/sentenceProb)* calcSum(y.r._1, pos))

      }
    }
  }

  class insideProb(c:Chart, m:Map[(Int,Int),Map[String,Double]]) {   
    for (item <- c.forest.reverse) {
      item match { 
	case x : lexical => None
	case y : singl => 
	  checkExist((y.b._1._1,y.b._1._3),y.b._1._2,y.b._3,m)	
	case z : binary => {
	  var f : Double = (z.bi._3 * m((z.bi._2._1._1,z.bi._2._1._3))
			    (z.bi._2._1._2) *
			    m((z.bi._2._2._1,z.bi._2._2._3))(z.bi._2._2._2))
	  checkExist((z.bi._1._1,z.bi._1._3), z.bi._1._2, f, m)	    
	}
      } 
    }
  }

  class outsideProb(x:Chart,o:Map[(Int,Int),Map[String,Double]],i:Map[(Int,Int),Map[String,Double]],is:Int,rules:Map[Rule,List[Rule]]) {
    
    private def addRule(r:Rule,pos:Rule){ 
      if (!rules.contains(r)){
	rules += (r -> List(pos))
      }
      else {rules(r) = pos::rules(r)}
    }

    private def rec(fir:(Int,String,Int),sec:((Int,String,Int),(Int,String,Int)),prob:Double){
      try {
	checkExist(
	(sec._1._1,sec._1._3),sec._1._2,prob*(i(sec._2._1,sec._2._3)(sec._2._2))*
	  o(fir._1,fir._3)(fir._2),o) 	
	checkExist(
	(sec._2._1,sec._2._3),sec._2._2,prob*(i(sec._1._1,sec._1._3)(sec._1._2))*
	  o(fir._1,fir._3)(fir._2),o)
      }
      catch { case eof : java.util.NoSuchElementException => None}
    }
  
    for (item <- x.forest) {
      item match { 	
	case x : lexical => None 
	case y : singl => 
	  addRule(PT(((y.b._1._2,y.b._2._2),y.b._3)),L((y.b._1._1,y.b._1._3)))
	case z : binary => 
	  addRule(B(((z.bi._1._2,z.bi._2._1._2,z.bi._2._2._2),z.bi._3)),
		  ML((z.bi._1._1,z.bi._1._3),(z.bi._2._1._1,z.bi._2._1._3),
		     (z.bi._2._2._1,z.bi._2._2._3)))
	  if(z.bi._1 == (0,"S",is)){
	    checkExist((z.bi._1._1,z.bi._1._3),z.bi._1._2,1.0,o)
 	    rec(z.bi._1,z.bi._2, z.bi._3)	    
 	  }
	  else { 
	   rec(z.bi._1,z.bi._2, z.bi._3)  

	  }

      }
    } 
  }

}

