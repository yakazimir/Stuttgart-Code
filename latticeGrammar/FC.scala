//IMPLEMENTATION OF CREATING FORMAL CONCEPTS
//BASED ON VILEM VYCHODIL 'A New Algorithm for Computing Formal Concepts'
//KYLE D. RICHARDSON - MAY 2012
//KYLE@IMS.UNI-STUTTGART.DE

import scala.io.Source
import scala.util.control.Breaks._
import scala.collection.mutable.{Map}

object FC { 

  var file = Source.fromFile("exampleObjects.txt").getLines.toList

  def compute_closure(B : List[Int], y : Int, n : Int, ob : objectAndAttrs) : List[Int]  = { 

    val D = Array.fill[Int](n+1)(1)

    for (i <- ob.rows(y)) { 
      var m : Boolean = true 
      breakable { 
	for (j <- (0 to n)) {
	  if ((B.contains(j)) && (!ob.context.contains((i,j)))) {
	    m = false; break
	  }
	}
      }
      if (m) { 
	for (z <- (0 to n)) {  
	  if (!ob.context.contains((i,z))) D(z) = 0
	}
      } 
    }
    return(D.zipWithIndex.filter(s => s._1 == 1).unzip._2.toList)
  }

  class intents(obj : objectAndAttrs, n : Int, attr : Set[Int]) {

    var i : Int = 1

    private def projectConcepts(B : List[Int]) {

      var U : List[Int] = List()
      println("INTENT:"+i+" "+B) 
      i += 1

      if (B != List()) {
	breakable {       
	  for (item <- B) {
	    if (U.isEmpty) U = obj.rows(item) 
	    else { 
	      U = U.intersect(obj.rows(item))
	      if (U == List()) break
	    }
	  }
	}
      }
      else U = obj.objectVals.keys.toList.sorted
      println("\t"+(U,B))
    }

    def generate_from(F : List[Int], y : Int) { 

      var B = F ; projectConcepts(B)
   
      if (B.toSet == attr || y > n) { return }
      
      for (j <- (y to n)) {
	if (!(B.contains(j))) {
	   
	  B ::= j 
	  var D = compute_closure(B, j, n, obj)
	  var skip = false  

	  breakable {	 
	    for (k <- (0 to j-1)) { 
	 
	      if (!(D.contains(k) && B.contains(k)) && 
		  (D.contains(k) || B.contains(k))) {
		skip = true; break
	      }
	    }
	  }
	  if (skip == false)  generate_from(D,j+1)
	  B = B.filter(s => s != j)
	}
      }
    } 
  }

  class objectAndAttrs(f : List[String]) { 

    var rows : Map[Int,List[Int]] = Map(); var rowVals : Map[Int,String] = Map()
    var objectVals : Map[Int,String] = Map()       
    var context = new scala.collection.mutable.ListBuffer[(Int,Int)]
    private var xM : List[String] = List(); private var objectNum : List[String] = List()
  

    private def computeVals(line : String) { 

      val commentOrEmpty = "(^\\#+.+|^\\n$)".r ; val attributeList = "(^attributes.+)".r
      val objectList = "(^objects.+)".r

      line match { 
	case commentOrEmpty(x) => None
	case attributeList(y) => { 
	  try {
	    xM = y.split(":")(1).split(" ").toList
	  } catch { 
	      case e:ArrayIndexOutOfBoundsException => println("attribute list error"); break
	  }
	}
	case objectList(z) => {
	    try { 
	      objectNum = z.split(":")(1).split(" ").toList
	    } catch { 
		case e:ArrayIndexOutOfBoundsException => println("object list error"); break
	    }
	}
	case _ => {

	  var s : Array[String] = line.split("--")
	  objectVals += (objectNum.indexOf(s(0)) -> s(0))

	  try {
	    for (attr <- s(1).split(" ")){
	  
	      if (rows.isEmpty) rows += (xM.indexOf(attr) -> List(objectNum.indexOf(s(0)))) 
	      else if (rows.contains(xM.indexOf(attr))) { 
		rows(xM.indexOf(attr)) = rows(xM.indexOf(attr))++List(objectNum.indexOf(s(0)))
	      }
	      else rows += (xM.indexOf(attr) -> List(objectNum.indexOf(s(0))))
	      context += ((objectNum.indexOf(s(0)),xM.indexOf(attr)))
	    }
	  } catch { 
	      case e : ArrayIndexOutOfBoundsException => 
	      println("line missing attributes: "+s(0)); break 
	  }
	}	 
      }
    }
    for (line <- f) computeVals(line)
    for (i <- xM) rowVals += (xM.indexOf(i) -> i) 
  }


  def main(args : Array[String]) { 

    var objects = new objectAndAttrs(file)
    val n = (objects.rows.keys.toList.length)-1
    val totalAttributes = (objects.rows.keys.toSet)   
    var ints = new intents(objects, n, totalAttributes)

    ints.generate_from(List(),0)

  }
}
