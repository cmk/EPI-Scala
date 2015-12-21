// Print first n rows of Pascal's Triangle
object pascalsTriangle {

  def pascal(r: Int)(c: Int): Int = {
    if (r == 0 || c == 0 || r == c) 1
    else pascal(r-1)(c-1) + pascal(r-1)(c)
  }

  def showTriangle(rows: Int) = {
    val lines = for {
      r <- 0 until rows
    } yield (0 to r) map pascal(r) mkString " "
    "\n" + (lines mkString "\n")
  }
  
  def main(args: Array[String]) {
    println(showTriangle(10)) 
  }
  
}