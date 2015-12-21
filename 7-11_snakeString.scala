//7-11 Return the snake string of a string 

object snakeString {

	def snakeString(s: String): String = {

		def foo(x:(Char,Int)) = (x._1,x._2%4)
	
		val rows = s.toCharArray.zipWithIndex.map(foo).groupBy {
			case (x,0) => "middle"
			case (x,1) => "upper"
			case (x,2) => "middle"
			case (x,3) => "lower"
		}  
	
		rows.valuesIterator.toList.reverse.flatten.map(_._1).mkString
	}

	def main(args: Array[String]) {
		println(snakeString("Hello World!")) 
	}
  
}