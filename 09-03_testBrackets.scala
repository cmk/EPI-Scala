// Determine whether an expression is parenthetically balanced
object testBrackets {

  def balance(chars: List[Char]): Boolean = {
  
    def foo(chars: List[Char], leftChars: List[Char]): Boolean = {
      if (chars.isEmpty) {
        leftChars.isEmpty //unmatched right char
      } else {
        val h = chars.head 
        h match {
          case '(' => foo(chars.tail, h :: leftChars)
          case '[' => foo(chars.tail, h :: leftChars)
          case '{' => foo(chars.tail, h :: leftChars)                     
          case ')' => leftChars.head == '(' && foo(chars.tail, leftChars.tail)
          case ']' => leftChars.head == '[' && foo(chars.tail, leftChars.tail)
          case '}' => leftChars.head == '{' && foo(chars.tail, leftChars.tail)
          case _ => false //illegal char input
          }
        }
    }
    foo(chars, List())  
  }
  
  def main(args: Array[String]) {
    println(balance("([{()}]){{{[]}}}".toList)) 
  }  

}