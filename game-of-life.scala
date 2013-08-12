import Math.abs

// Game of Life logic
case class Cell(x: Int, y: Int, isLife: Boolean)

def neighbours(grid: List[Cell], cell: Cell) =  grid filter {
	neighbour => abs(neighbour.x - cell.x) == 1 || abs(neighbour.y - cell.y) == 1 
} 

def live(cells: List[Cell]) = cells filter { _.isLife }

def tick(grid: List[Cell]) = {
	grid map { cell =>
		live(neighbours(grid, cell)).size match {
			case 2 => cell
			case 3 => if (!cell.isLife) Cell(cell.x, cell.y, true) else cell
			case _ => if (cell.isLife) Cell(cell.x, cell.y, false) else cell
		}
	}
}
// End of Game of Life logic

// Console output & test code
def toString(grid: List[Cell]) = {

	grid map { cell =>
		if (cell.isLife)
			if (cell.x == 1) "*\n" else "*"
		else
			if (cell.x == 1) "-\n" else "-"
	}
}

val seed = Cell(0, 0, true) :: Cell(1, 0, true) :: Cell(0, 1, true) :: Cell(1, 1, false) :: Nil


toString(seed) foreach print
var i = 0;
for (i <- 1 to 10)
	toString(tick(seed)) foreach print
