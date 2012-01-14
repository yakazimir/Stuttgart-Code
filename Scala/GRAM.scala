import scala.collection.mutable.{Set,Map,HashSet}

object GRAM { 
  
  abstract class Entry
  case class R(binary:Product,prob:Double) extends Entry 
  case class L(lexical:String,prob:Double) extends Entry
  
  class Grammar {
    
    val productions : Map[String,List[Entry]] = Map(
      "S" -> List(R(("N","V"),1.0)), 
      "V" -> List(R(("V","OP"),0.1), 
		  R(("V","N"),0.4), 
		   L("eats",0.2), 
		   L("loves",0.1), 
		   L("hates",0.2), 
		   L("pizza",0.2)), 
      "N" -> List(R(("N","P"),0.2), 
		   L("she",0.1), 
		   L("he", 0.1), 
		   L("pizza",0.1), 
		   L("anchovies",0.2), 
		   L("meat", 0.2), 
		   L("bread",0.1), 
		   L("cheese",0.1)), 
      "P" -> List(R(("PP","N"),1.0)), 
      "PP"-> List( L("without",0.4), 
		   L("with",0.2), 
		   L("on", 0.4)), 
      "OP"-> List(R(("N","P"),1.0)))


    val nTerminals = productions.keys  
    val terminals = new HashSet[String]
    val subList = new HashSet[Product]
    var tReverseMap : Map[String,Set[Product]] = Map() 
    var rReverseMap : Map[Product, Set[Product]] = Map()  


    def init(){ 
      def add[T](z:String,x:T,pr:Double,y:Map[T,Set[Product]]):Map[T,Set[Product]]={ 
	if (!y.contains(x)) {y += (x -> Set((z,pr)))}
	else {y(x) += ((z,pr))}
	y }
      for(z <- nTerminals){
	for (part <- productions(z)){ 
	  part match { 
	    case x : R => add[Product](z,x.binary,x.prob,rReverseMap); subList += x.binary
	    case y : L => add[String](z, y.lexical,y.prob,tReverseMap); terminals += y.lexical
	  }	  
	}
      }
    }
  }
}
