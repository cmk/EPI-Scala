//9-2 Evaluate comma-separated reverse Polish notation expressions

object evaluateRPN {

	type Stack = scala.collection.mutable.Stack[Int]

	def evaluate(expression: String): Int = { 
	
		def foo(expr: List[String], stack: Stack): Int = {
		
			expr match { 
				case Nil => stack.pop
				case "+"::_ => foo(expr.tail, stack.push(stack.pop + stack.pop))
				case "-"::_ => foo(expr.tail, stack.push(stack.pop - stack.pop))
				case "*"::_ => foo(expr.tail, stack.push(stack.pop * stack.pop))
				case "/"::_ => foo(expr.tail, stack.push(stack.pop / stack.pop))
				case x::_  => foo(expr.tail, stack.push(x.toInt))
			}
		
		}
		foo(expression.split(',').toList, new Stack)
	}	

	def main(args: Array[String]) {
		val s = "1,1,+,-2,*,-4,/"
		println(evaluate(s)) 
	}
  
}