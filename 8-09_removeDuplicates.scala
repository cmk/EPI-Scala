//8-9 remove duplicates from a sorted list

object removeDuplicates {

	def removeDuplicates[A](list: List[A]): List[A] =
		list.foldLeft(List[A]()) { (r,c) => r match {
			case Nil => c :: r
			case x :: xs => if (x == c) r
							else c :: r
			}

		}.reverse
		
	def main(args: Array[String]) {
		val l = List(1,1,1,2,3,4,4,4,5,5,6)
		println(l.mkString(","))
		println(removeDuplicates(l).mkString(","))
	}

}