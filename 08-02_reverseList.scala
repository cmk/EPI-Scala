//8-2 reverse a singly linked list

object reverseList {

	def reverse[A](list: List[A]): List[A] = list.foldLeft(List[A]())((r,c) => c :: r)
	
	def main(args: Array[String]) {
		val l = List(1,2,3,4,5,6,7,8,9,0)
		println(l.mkString(","))
		println(reverse(l).mkString(","))
	}
  
}	