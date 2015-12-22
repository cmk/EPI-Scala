//7-12 implement run-length encoding

object implementRLE {

	def encode[A](list: List[A]): List[(Int, A)] =
		list.foldRight(List[(Int, A)]()){ (c, r) =>
			r match {
				case (count, value) :: tail =>
					if (value == c) (count + 1, c) :: tail
					else            (1, c) :: r
				case Nil => (1, c) :: r
			}
		}

	def decode[A](list: List[(A, Int)]): List[A] =
		list.foldRight(List[A]()){ (c, r) =>
			var result = r
			for (_ <- 1 to c._2) result = c._1 :: result
			result
		}

	def main(args: Array[String]) {
		val l = List(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
		println(l.mkString(","))
		println(encode(l).mkString(","))
		println(decode(encode(l)).mkString(","))
	}

}