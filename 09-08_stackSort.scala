//9-8 sort a stack 
//NB- this can be done as an insertion sort using the foldLeft API.

object stackSort {

	def stackSort[A <% Ordered[A]](list: List[A]): List[A] =
		list.foldLeft(List[A]()) { (r,c) =>
			val (front, back) = r.span(_ > c)
			front ::: c :: back
		}

	def main(args: Array[String]) {
		val l = List(9,3,4,2,6,4,8,1,5,63)
		println(l.mkString(","))
		println(stackSort(l).mkString(","))
	}

}