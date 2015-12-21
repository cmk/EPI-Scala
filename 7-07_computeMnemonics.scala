// Computes all phone keypad letter combinations for a given string of digits

object computeMnemonics {

	val keypad = Map('2'->"abc",'3'->"def",'4'->"ghi",'5'->"jkl",'6'->"mno",'7'->"pqrs",'8'->"tuv",'9'->"wxyz")
	
	val charsForNum: Map[Char,Seq[Char]] = keypad.mapValues(_.toList)
	
	def encode(number: String): List[List[Char]] = {
		if (number.isEmpty) List(List())
		else
			for {
				rest <- encode(number.tail)
				chars <- charsForNum(number.head)
			} yield chars :: rest
	}
	
	def main(args: Array[String]) {
		println(encode("22").map(_.mkString.mkString(", ")) 
	}
  
}
