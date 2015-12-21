//6-19 Rotate a n x n 2D matrix representing an image in place by 90 degrees (clockwise). 

object rotate2D {

	def swap(arr: => Array[Array[Int]], i: Int, j: Int, ii: Int, jj: Int): Unit = {
		val tmp: Int = arr(i)(j)
		arr(i)(j) = arr(ii)(jj)
		arr(ii)(jj) = tmp
	}

	def transpose(arr: => Array[Array[Int]]): Unit = {
		for {i <- (0 until arr(0).length) 
			 j <- (i until arr(0).length) 
		} swap(arr,i,j,j,i)
	}

	def flip(arr: => Array[Array[Int]]): Unit = {
		for {i <- (0 until arr(0).length) 
			 j <- (0 until arr(0).length / 2)
		} swap(arr,i,j,i,arr(0).length - j - 1)
	}

	def rotate(arr: => Array[Array[Int]]): Unit = {
		transpose(arr)
		flip(arr)
	}

	def arrayToString(arr: Array[Array[Int]]) : String = {
		val str = for (l <- arr) yield l.mkString("", ",", "")
		str.mkString("",",\n","")
	}
	
	def main(args: Array[String]) {
		val n: Int = 4
		var arr = Array.ofDim[Int](n, n)
		arr(0)(0) = 1
		arr(1)(1) = -1
		println(arrayToString(arr))
		println()
		rotate(arr)
		println(arrayToString(arr))
	}
  
}