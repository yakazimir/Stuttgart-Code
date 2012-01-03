
import scala.collection.mutable.Set

object Grammar { 
  
  abstract class Entry 
  case class NT(rew:Product,p:Double) extends Entry 
  case class T(t1:String, p:Double) extends Entry

  val productions = Map(
    "S" -> List((("N","V"),1.0)), 
    "V" -> List((("V","N"),0.6), 
		(("V","PP"),0.4),
		(("eats"),0.4), 
		(("loves"),0.3), 
		(("hates"),0.3)),
    "N" -> List((("N","P"),0.2), 
		(("she"),0.1), 
		(("he"),0.1), 
		(("pizza"),0.1),
		(("anchovies"),0.1),
		(("meat"),0.1), 
		(("bread"), 0.1), 
		(("cheese"),0.1)),
    "P" -> List((("PP","N"),1.0)),
    "PP"-> List((("without"),0.4),
		(("with"),0.2), 
		(("on"),0.4)),
    "OP"-> List((("N","P"),1.0)))

  val Nterminals = productions.keys
  var tList : Set[Any] = Set()
  var revMap : Map[Any,Set[Product]] = Map()

  def init  = for ((x,y) <- productions) {
    for (item <- y){
      tList += item._1
      if (!revMap.contains(item._1)){ 
	revMap += (item._1 -> Set((x,item._2)))
      }
      else {revMap(item._1) += ((x,item._2))}
    }
  }


}

















      //else revMap(item._1) += 
	//revMap(""
      //}
    //}
  //}

 

       
      //else: 
	  
      //revMap += (item._1 -> List(x,item._2))
       
       

       










    //return A : Set[Any
//print(A)
 

  

      //println(item._1)
      //for (sitem <- item.productIterator){ 
	//println(sitem._0)
      //}
   //  productions.get(name) match {
//       case Some(code) => 
// 	name + " has " + code 
//       case None => 
// 	"Unknown code " 
//     }

//var termMap:Map[Any,Any] = Map()
    //productions foreach( 
    // (t2) => t2.map(t2._1 -> t2._2))

//var termMap:Map[String,Int] = _
    //for ((t1,t2) <- productions) termMap += Map(t1 -> 2)   
    //productions.foreach(t1 => termMap += Map(t1._1 -> t1._2))


//   val productions = Map(
//       "S" -> (NT(("N","V"),1.0)), 
//       "V" -> (NT(("V","N"),0.6), 
// 	      NT(("V","PP"),0.4),
// 	       T(("eats"),0.4), 
// 	       T(("loves"),0.3), 
// 	       T(("hates"),0.3)),
//       "N" -> (NT(("N","P"),0.2), 
// 	       T(("she"),0.1), 
// 	       T(("he"),0.1), 
// 	       T(("pizza"),0.1),
// 	       T(("pizza"),0.1),
// 	       T(("anchovies"),0.1),
// 	       T(("meat"),0.1), 
// 	       T(("bread"), 0.1), 
// 	       T(("cheese"),0.1)),
//       "P" -> (NT(("PP","N"),1.0)),
//       "PP"->  (T(("without"),0.4),
// 	       T(("with"),0.2), 
// 	       T(("on"),0.4)),
//       "OP"-> (NT(("N","P"),1.0)))

    // def Q = {
//       println(x)
//       println(y(0))
//     }
//     Q
