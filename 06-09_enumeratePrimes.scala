//6-9 enumerate first n primes 

object enumeratePrimes {

	def from(n: Int): Stream[Int] = n #:: from(n+1)

	def sieve(s: Stream[Int]): Stream[Int] = {
		s.head #:: sieve(s.tail filter (_ % s.head != 0))
	}
	
	def enumeratePrimes(n: Int): List[Int] = sieve(from(2)).take(n).toList

	def main(args: Array[String]) {
		println(enumeratePrimes(25).mkString(","))
	}

}