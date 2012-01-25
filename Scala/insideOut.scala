import CYK._
import GRAM.Grammar
import scala.collection.mutable.{Set,Map, HashSet}
//import scala.io.Source
//import scala.collection.mutable.{Set,Map}

object insideOut { 

  var inside : Map[(Int,Int),Map[String, Double]] = Map()
  var outside : Map[(Int,Int),Map[String,Double]] = Map()


  private def checkExist(x:(Int,Int),y:String,y2:Double,z:Map[(Int,Int),Map[String,Double]]){
    
    if (!z.contains(x)){
      z += (x -> Map(y -> y2))
    }
    else {
      if (z(x).contains(y)){ 
	println(y)
      } 
      else {z(x) += (y -> y2)}
    }

  }

  class insideOut { 

  }

  class insideProb(x:Chart) {
    
    var forest = x.forest.reverse    
    for (item <- forest) {
      item match { 
	case x : lexical => None
	case y : singl => 
	  checkExist((y.b._1._1,y.b._1._3),y.b._1._2,y.b._3,inside)	
	case z : binary => 
	  None
	  //if checkExit(z.bi._1._1
	  
      }

    }
    println(inside) 
  }

  class outsideProb(x:Chart) {
    
    
    

  }

}
