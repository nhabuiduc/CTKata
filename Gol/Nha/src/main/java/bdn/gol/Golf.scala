package bdn.golf

class Golf {
  def tick(matrix: Seq[Seq[Int]]): Seq[Seq[Int]] =
    for (i <- 0 to matrix.length - 1)
      yield for (j <- 0 to matrix.length - 1)
        yield evolve(matrix(i)(j),getLiveNeighboursCount(matrix,i,j))

    def evolve(cell:  Int, aliveNeighbours:Int):  Int =
      cell match {
        case 0 => if (aliveNeighbours == 3) 1 else 0
        case _ => aliveNeighbours match {
          case 2 | 3 => 1
          case _ => 0
        }
      }

    def getLiveNeighboursCount(matrix: Seq[Seq[Int]], line: Int, column: Int) = {
      val neighbours = for (
        i <- Math.max(line - 1, 0) to Math.min(line + 1, matrix.length - 1);
        j <- Math.max(column - 1, 0) to Math.min(column + 1, matrix.length - 1)
      ) yield matrix(i)(j)

      neighbours.sum - matrix(line)(column)
    }
}
