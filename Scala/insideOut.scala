import CYK._
import GRAM.Grammar
import scala.collection.mutable.{Set,Map, HashSet}
//import scala.io.Source
//import scala.collection.mutable.{Set,Map}

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

  class insideOut { 

  }

  class insideProb(x:Chart) {
    
    var inside : Map[(Int,Int),Map[String, Double]] = Map()

    for (item <- x.forest.reverse) {
      item match { 
	case x : lexical => None
	case y : singl => 
	  checkExist((y.b._1._1,y.b._1._3),y.b._1._2,y.b._3,inside)	
	case z : binary => {
	  var f : Double = (z.bi._3 * inside((z.bi._2._1._1,z.bi._2._1._3))(z.bi._2._1._2) *
			    inside((z.bi._2._2._1,z.bi._2._2._3))(z.bi._2._2._2))
	  checkExist((z.bi._1._1,z.bi._1._3), z.bi._1._2, f, inside)
	    
	}
      } 
    }
  }

  class outsideProb(x:Chart) {

    var outside : Map[(Int,Int),Map[String,Double]] = Map()
    
    for (item <- x.forest) {
      
      



    }
  
  }

}
