//16-9 Create a Sudoku solver

object sudokuSolver {

	def checkX(board: List[(Int,Int)], next: (Int,Int)): Boolean = {
		def check(b: Boolean, tuple: (Int,Int)) = 
			{ b && ((tuple._1 / 9 == next._1 / 9 && tuple._2 != next._2) || tuple._1 / 9 != next._1 / 9) }
		board.foldLeft(true)(check)      
	}

	def checkY(board: List[(Int,Int)], next: (Int,Int)): Boolean = {
		def check(b: Boolean, tuple: (Int,Int)) = 
			{ b && ((tuple._1 % 9 == next._1 % 9 && tuple._2 != next._2) || tuple._1 % 9 != next._1 % 9) }
		board.foldLeft(true)(check)    
	}

	def checkT(board: List[(Int,Int)], next: (Int,Int)): Boolean = {
		def tile(index: Int): Int = {
			val y: Int = index % 9
			val x: Int = index / 9
			(x,y) match {
				case _ if (0 to 2 contains y) && (0 to 2 contains x) => 0
				case _ if (3 to 5 contains y) && (0 to 2 contains x) => 1
				case _ if (6 to 8 contains y) && (0 to 2 contains x) => 2
				case _ if (0 to 2 contains y) && (3 to 5 contains x) => 3
				case _ if (3 to 5 contains y) && (3 to 5 contains x) => 4
				case _ if (6 to 8 contains y) && (3 to 5 contains x) => 5
				case _ if (0 to 2 contains y) && (6 to 8 contains x) => 6
				case _ if (3 to 5 contains y) && (6 to 8 contains x) => 7
				case _ if (6 to 8 contains y) && (6 to 8 contains x) => 8
			}
		} 
		def check(b: Boolean, tuple: (Int,Int)) = 
			{ b && ((tile(tuple._1) == tile(next._1) && tuple._2 != next._2) || tile(tuple._1) != tile(next._1)) }
		board.foldLeft(true)(check)    
	}

	def notPlayed(board: List[(Int,Int)], index: Int): Boolean = {
		!(board map {x => x._1} contains index)
	}

	def isLegal(board: List[(Int,Int)], next: (Int,Int)): Boolean = {
		checkX(board, next) && checkY(board, next) && checkT(board, next) 
	}

	//generate all possible valid Sudoku solutions, this is for reference only (compare 16-02)
	def sudokuAll(index: Int): Set[List[(Int,Int)]] = {
		if (index == -1) Set(List())
		else
			for {
				board <- sudokuAll(index-1)
				k <- 0 until 9
				if isLegal(board, (index,k))
			} yield (index,k)::board
	}

	def sudokuSolve(initial: List[(Int,Int)]): Set[List[(Int,Int)]] = {
		val indices = (0 until 81) filter {notPlayed(initial,_)} toList
		def sudokuIter(indices: List[Int]): Set[List[(Int,Int)]] = indices match {
			case Nil => Set(initial)
			case index::tail => for {
				board <- sudokuIter(tail)
				k <- 1 until 10
				if isLegal(board, (index,k))
			} yield (index,k)::board        
		}
		sudokuIter(indices)
	}

	def sudokuPlot(board: List[(Int,Int)]): String = {
		val out = Array.ofDim[Int](9,9)
		for {move <- board} out(move._1 / 9)(move._1 % 9) = move._2
		out.map({_.mkString(" ")}).mkString("\n")
	}

	def main(args: Array[String]) {
		//example problem from 6-17
		val initial = List((0,5),(1,3),(4,7),(9,6),(12,1),(13,9),(14,5),(19,9),(20,8),(25,6),(27,8),(31,6),(35,3),(36,4),(39,8),(41,3),(44,1),(45,7),(49,2),(53,6),(55,6),(60,2),(61,8),(66,4),(67,1),(68,9),(71,5),(76,8),(79,7),(80,9))
		println(sudokuPlot(initial))
		println("\n")
		println(sudokuPlot(sudokuSolve(initial).toList.head))
	}
  
}