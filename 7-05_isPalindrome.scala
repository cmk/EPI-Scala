// Determine whether a string is palindromic

object isPalindrome {

	def isPalindrome(s: String): Boolean = {
    	val arr = s.toLowerCase.toCharArray.filter(_.isLetter)    
    	def foo(arr: Array[Char]): Boolean = {
        	val l = arr.length 
        	l match {
            	case 0 => true
            	case 1 => true
            	case _ => arr(0) == arr(l-1) && foo(arr.drop(1).take(l-2))
        	}
    	}
    	foo(arr)
	}

	def main(args: Array[String]) {
		val s = "Able was I, ere I saw Elba."
		println(isPalindrome(s)) 
	}
  
}


