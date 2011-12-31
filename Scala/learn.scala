// // 

// var flag = true 
// def f(n : Int) = { 
//   var k = 0 
//   if (flag) k=n else k=2*n 
//   flag = ! flag  
//   k 
// } 

// println(f(1) +f(2)) 
// println(f(2) + f(1))


// class cell{ 
//   var contents : Int = 0 
//   def get() = contents 

//   def set(n : Int) = {
//     contents = n 

//   }

// }



class learn (){
  var day : String = _ 
  var month : String = _ 
  var year : Int = _
    
  def set(x : String, y : String, z : Int){ 
    day = x  
    month = y 
    year = z 
  }
  def output() = println(day+month+year)
}

object Static {
  def main(args: Array[String]){
    var a = new learn
    a.set("Monday","January",2001)
    println(a.output)

  }
}


