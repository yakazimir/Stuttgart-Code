import CYK._
import GRAM.Grammar
import scala.collection.mutable.{Set,Map, HashSet}

object insideOut { 
  
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

  class insideOut(x:Chart,inputSize:Int){ 
    
    var inside : Map[(Int,Int),Map[String,Double]] = Map() 
    var outside : Map[(Int,Int),Map[String,Double]] = Map() 
    
    val insideVals = new insideProb(x,inside)
    val outsideVals = new outsideProb(x,outside, inside, inputSize)

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

  class outsideProb(x:Chart,o:Map[(Int,Int),Map[String,Double]],i:Map[(Int,Int),Map[String,Double]],is:Int) {
    
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
	case y : singl => None 
	case z : binary => 
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
