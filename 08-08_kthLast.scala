//8-8 return the k-th *last* element of a linked list 
//NB- i like the recursive elegance of this solution but it uses O(k) space, which is suboptimal. an imperative double pointer solution using O(1) space would be more efficient.

object kthLast {

	def kthLast[A](l: List[A], k: Int) = {
 
		def getK[A](elt: A, cache: List[A]): List[A] = 
			if (cache.length == k) cache
			else elt :: cache
	
		l.foldRight(List[A]())(getK) head 
	}

	def main(args: Array[String]) {
		val l = List(10,9,8,7,6,5,4,3,2,1)
		println(l.mkString(","))
		println(kthLast(l,4))
	}
	
}