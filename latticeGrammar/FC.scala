//IMPLEMENTATION OF CREATING FORMAL CONCEPTS
//BASED LARGELY ON VILEM VYCHODIL 'A New Algorithm for Computing Formal Concepts'
//KYLE D. RICHARDSON - MAY 2012
//KYLE@IMS.UNI-STUTTGART.DE

import scala.io.Source
import scala.util.control.Breaks._
import scala.collection.mutable.{Map,HashSet}

object FC { 
  
  abstract class Concept
  case class ConceptTuple(e: List[String], i: List[Int]) extends Concept
  case class ConceptList(l : List[Concept]) extends Concept
  //standard test file
  //var file = Source.fromFile("newOne.txt").getLines.toList
  //var file = Source.fromFile("exampleObjects.txt").getLines.toList
  //var file = Source.fromFile("newTest.txt").getLines.toList
  //var file = Source.fromFile("synConcepts.txt").getLines.toList
  //testing altering
  //var file = Source.fromFile("altering.txt").getLines.toList
  //var file = Source.fromFile("makeLarge.txt").getLines.toList
  //var file = Source.fromFile("largerTest.txt").getLines.toList
  var file = Source.fromFile("testing.txt").getLines.toList
  //var file = Source.fromFile("testing1.txt").getLines.toList

  var conceptList = new HashSet[Concept]
  def time(f: => Unit) = { 
    val s = System.currentTimeMillis; f; System.currentTimeMillis - s
  }

  def compute_closure(B : List[Int], y : Int, n : Int, ob : objectAndAttrs) : List[Int] = { 
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
      m match {
	case true => {
	  for (z <- (0 to n)) {  
	    if (!ob.context.contains((i,z))) D(z) = 0
	  }
	}
	case _ => Nil
      }
    }
    return(D.zipWithIndex.filter(s => s._1 == 1).unzip._2.toList)
  }
  class intents(obj : objectAndAttrs, n : Int, attr : Set[Int]) {
    var i : Int = 1

    private def semP(l : List[String], u : List[Int]){
      var totalNum = l.map(x=>obj.objectFreq(x)).foldRight(0)(_+_)
      println("\t\tTotal Number Word(s) Seen: "+totalNum)
      var b = l.map(x=>obj.objectSemFreq(x))
      if (l.length != obj.objectVals.keySet.size){ 
	try { 
	  var l : Map[String,Double] = b.head
	  for (i<- b.takeRight(b.length-1)) {
	    l = l ++ i.map { case (k,v) => k -> (v + l.getOrElse(k,0.0)) }
	  }
	  for ((i,j) <- l.toList.sortBy(_._2).reverse.toMap) {
	    println("\t\t"+i+": "+(j/totalNum.toDouble))   
	  }
	} catch { 
	    case e : NoSuchElementException => 
	      if (b == List()) {println("\t\tbottom of lattice")}
	    case _ => println("issue in print back"); break		   
	}
      }
      else println ("\t\tTop of lattice")
    }

    private def projectConcepts(B : List[Int]) {
      var U : List[Int] = List(); var Z : List[String] = List()
      //for printing
      //println("INTENT:"+i+" "+B); 
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
	  if (U != List()) Z = U.map(s => obj.objectVals(s))
	}
      }
      else Z = obj.objectVals.keys.toList.map(s => obj.objectVals(s))
      //for printing
      //println("\tConcept: "+(Z,B))
      //println(Z.length)
      println("\tConcept "+i+": "+Z)
      semP(Z, U)
      //for storing later
      //conceptList += ConceptTuple(Z,B)      
    }

    def generate_from(F : List[Int], y : Int) { 
      var B = F ; projectConcepts(B) 
      if (B.toSet == attr || y > n) { return } 
      for (j <- (y to n)) {
	B.contains(j) match {	   
	  case false => { 
	    B ::= j ; var D = compute_closure(B, j, n, obj) 
	    var skip = false  	    
	    breakable {	 
	      for (k <- (0 to j-1)) { 
		if (!(D.contains(k) && B.contains(k)) && 
		    (D.contains(k) || B.contains(k))) {
		      skip = true; break
		    }
	      }
	    }
	    skip match { 
	      case false => generate_from(D,j+1)
	      case _ => None
	    }
	    B = B.filter(s => s != j)
	  }
	  case true => None
	}
      }
    } 
  }
  class objectAndAttrs(f : List[String]) { 
    var rows : Map[Int,List[Int]] = Map(); var rowVals : Map[Int,String] = Map()
    var objectVals : Map[Int,String] = Map()
    var objectFreq : Map[String,Int] = Map()
    var objectSemFreq : Map[String,Map[String, Double]] = Map()
    var context = new scala.collection.mutable.ListBuffer[(Int,Int)]
    private var xM : List[String] = List(); private var objectNum : List[String] = List()
    var objectAttrs : Map[Int,List[Int]] = Map()  
    private def computeVals(line : String) { 
      def fillMaps(map : Map[Int,List[Int]], key : Int, val1 : Int) {
	map.contains(key) match { 
	  case true => map(key) = map(key)++List(val1) 
	  case false => map += (key -> List(val1))
	}
      }
      val commentOrEmpty = "(^\\#+.+|^\\n$)".r ; val attributeList = "(^attributes.+)".r
      val objectList = "(^objects.+)".r

      line match { 
	case commentOrEmpty(x) => Nil
	case attributeList(y) => { 
	  try {
	    xM = y.split(":")(1).split(";").toList
	  } catch { 
	      case e:ArrayIndexOutOfBoundsException => println("attribute list error"); break
	  }
	}
	case objectList(z) => {
	    try { 
	      objectNum = z.split(":")(1).split(";").toList
	    } catch { 
		case e:ArrayIndexOutOfBoundsException => println("object list error"); break
	    }
	}
	case _ => {
	  var s : Array[String] = line.split("--")
	  var wordName : String = s(0).split('.')(0)
	  var freq : Int = s(0).split('.')(1).toInt
	  try { 
	    objectVals += (objectNum.indexOf(wordName) -> wordName)
	    objectFreq += (wordName -> freq)
	  } catch { 
	      case e : ArrayIndexOutOfBoundsException => 
		println("error with format, line: "+s(0)); break
	  }
	  try {
	    var attributeSide : Array[String] = s(1).split("<<")(0).split(";") 
	    var semValSide : Array[String] = s(1).split("<<")(1).split(";")
	    for (attr <- attributeSide) {
	      fillMaps(rows, xM.indexOf(attr), objectNum.indexOf(wordName))    
	      fillMaps(objectAttrs, objectNum.indexOf(wordName), xM.indexOf(attr))
	      context += ((objectNum.indexOf(wordName),xM.indexOf(attr)))
	    }	
	    for (sems <- semValSide){
	      var semV : String = sems.split("=")(0)
	      var cV : Double = sems.split("=")(1).toDouble
	      objectSemFreq.contains(wordName) match {
		case false => 
		  objectSemFreq += (wordName -> Map(semV -> cV))
		case _ => 
		  objectSemFreq(wordName) += (semV -> cV) 
	      }
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
    //println(objects.objectVals)
    var timeFun : Double = time(ints.generate_from(List(),0)); println(timeFun/1000.0)
    //println(objects.objectSemFreq)

  }
}



















// class filter(h : HashSet[Concept], o : objectAndAttrs) { 

//     //eventually make it more incrememntal

//     var rev : Map[String,Int] = o.objectVals map {_.swap}

//     def powerSet[A](s: Set[A]) =
//       s.foldLeft(Set(Set.empty[A])) {
// 	(set, element) =>
//            set union (set map (_ + element))
//     }
//     for (i <- h) i match {      
//       case x : ConceptTuple => {
// 	println(x)
// 	var s : Set[Int] = Set()
// 	var pCount : Float = 0 
// 	var z = powerSet(x.e.toSet) 
// 	//println(z)
// 	//println(x.i)
// 	for (valu <-z) {

// 	  if (valu == Set()) { 
// 	    if (x.i == List()) pCount += 1
// 	    else {}
// 	  }
// 	  else { 
	    
// 	    for (obj <- valu) {
// 	      var oo = o.objectAttrs(rev(obj)).toSet
// 	      if (s == Set()) s = oo 
// 	      else (s = s.intersect(oo))	    
// 	      //print("\t"+oo)	      
// 	    }
// 	    //println(" intersection: "+s)
// 	    //println("\n")	    	    

// 	    if (s == x.i.toSet) pCount += 1 
// 	    else {} 
// 	    s = Set()
// 	  }
// 	}
	
// 	// try { println(pCount/z.size.toFloat) }
// 	// catch { case e : ArithmeticException => None }

// 	println("\t Score: "+pCount/z.size.toFloat)
// 	pCount = 0
//       }
//       case _ => None 

//     }

//   }
