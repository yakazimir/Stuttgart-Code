import GRAM.Grammar
import scala.util.control.Breaks._
import scala.collection.mutable.{Set,Map}
import scala.io.Source

import java.util.Formatter._
//import java.lang.Runtime._


object CYK {
  
  val gram = new Grammar; gram.init
  val file = Source.fromFile("sentences.txt").getLines.toList

  def time(f: => Unit)= {
	val s = System.currentTimeMillis; f
	System.currentTimeMillis - s
  }
   
  def makeP(acc:Set[String],se:Set[String]) = 
    for (a <- acc; s <- se) yield {(a,s)}

 
  class Parser (gram : Grammar,in : Input,table : Chart){
    
    val input = in.pos 
    
    private def allSpan(k : Int, z : Int, j : Int){
      try {
	var x = table.position((k,z)); var y = table.position((z,j)) 
	def prod(pair : Set[String]) = { 
	  var ter : Set[String] = Set() 
	  for (i <- pair) ter += i;ter
	}
	for(pair <- makeP(prod(x),prod(y))) table.Add((k,j),pair,z)
      }
      catch { 
	case eof: java.util.NoSuchElementException => 
	  None
      }
    }

    private def inParse(i : Int,j : Int) { 
      var k : Int  = j - 2 
      while (k > - 1){
	var z : Int = k+1
	while (z < j) {
	  allSpan(k,z,j); z += 1	
	}
	k -= 1
      }
    }
    
    def Parse {
      for (word <- input) {
	if ((gram.tList).contains(word._1)){ 
	  var i = word._2; var j = i+1
	  table.Add((i,j),word._1, 0); inParse(i,j)
	}     
	else {println("not in grammar")} 
      }     
    }
  }

  class Input(x : String) { 

    val pos = ((x.split(" ")).toList).zipWithIndex
    val original = "input: "+ x

    def inWSpans {
      for (word <- pos) {
	var f = word._2; print(f+" "+word._1+" ")
      }
      print(pos.length+"\n")
    } 
  }

  class Chart { 

    val position : Map[Product,Set[String]] = Map()
    var forest : List[Product] = List()

    def printForest = { 
      val string = "%-15s ==> %-20s %-5s"; print("\n")
      for (entry <- forest){ 
	println("\t"+string.format((entry.productElement(0)).toString,
	  (entry.productElement(1)).toString,
	  (entry.productElement(2)).toString))
      }
      println("\n")
    }

    private def pForest(entry : Set[Product],in : Any, pos : Product, mid : Int){ 
      
      var i = pos.productElement(0); var j = pos.productElement(1)
      
      if (mid == 0) {
	var lexItem = ((i,in,j),(in),1.0)
	forest = lexItem::forest
	for (value <- entry){
	  var tVal = value.productElement(0)
	  var prob = value.productElement(1) 
	  forest = ((i,tVal,j),(i,in,j),prob)::forest
	}
      }
      else {	
	for (value <- entry) { 
	  var tVal = value.productElement(0);var prob = value.productElement(1)
	  var f = in.asInstanceOf[Product].productElement(1) //fix this 
	  var s = in.asInstanceOf[Product].productElement(0)// fix this
	  forest = ((i,tVal,j),((i,f,mid),(mid,s,j)),prob)::forest
	}
	
      }
    }

    def Add(pos: Product, in : Any, mid : Int) {
      
      def S(entry : Set[Product]) = {
	var tableSet : Set[String] = Set() 
	for (piece <- entry){ 
	  tableSet += (
	    piece.productElement(0)).asInstanceOf[String]
	}
	tableSet
      }

      (gram.revMap).get(in) match {
	case Some(entry) => 
	  if (position.contains(pos)){
	    position(pos) ++ S(entry)
	    pForest(entry, in, pos, mid)
	  }
	  else {
	    position += (pos -> S(entry))
	    pForest(entry, in, pos, mid)
	  }
	case None => 
	  None
      }
    }

    def printChart(in : Input)  = { 
      print("input: "); in.inWSpans

      for (i <- 0 to ((in.pos).length)-1) { 
	print(i)
	for (j <- 1 to ((in.pos).length)){ 
	  position.get((i,j)) match{ 
	    case Some(item) => print(" "+item.mkString(",")+" "+j)
	    case None => print(" "+" "+" "+j)
	  }
	}
	print("\n") 
      }
      print("\n")
    }

  } 

  def main(args: Array[String]){ 
    for (sentence <- file) { 
      var chart = new Chart; var input = new Input(sentence.toLowerCase) 
      var parser = new Parser(gram,input, chart) 
      var timeFun : Double = time(parser.Parse)
      
      print("INPUT: "); input.inWSpans
      println("CPU Time: " + timeFun / 1000.0) 
      chart.printForest
    }

  }
}    
