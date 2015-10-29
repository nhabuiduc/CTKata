package bdn.goli
import bdn.goli.CellStatus.CellStatus

object  CellStatus extends Enumeration {
  type CellStatus = Value
  val Dead, Alive = Value
}

case class Cell(x: Int, y: Int,status: CellStatus = CellStatus.Alive)
case class Neighbours(cell:Cell,neighbours:Seq[Cell])

class Goli {
  import CellStatus._

  def tick(liveCells: Seq[Cell]): Seq[Cell] = {

    val cellAndNeighbours = liveCells.map(c => Neighbours(c, getNeighbours(c, liveCells)))

    val stillAliveCells = cellAndNeighbours
                        .map(f => evolve(f.cell, f.neighbours))
                        .filter(f => f.status == Alive)

    val comeAliveCells = cellAndNeighbours
                        .flatMap(f => f.neighbours)
                        .filter(f=>f.status == Dead)
                        .groupBy(f => f)
                        .filter(f => f._2.size == 3)
                        .map(f => Cell( f._1.x, f._1.y))

    stillAliveCells ++ comeAliveCells
  }

  def evolve(cell: Cell, neighbours: Seq[Cell]): Cell = {

    val liveCellTotal = getNeighbours(cell, neighbours).count(f => f.status == Alive)
    Cell( cell.x, cell.y, if(liveCellTotal == 2 || liveCellTotal ==3)  Alive else Dead)
  }

  def getNeighbours(current: Cell, liveCells: Seq[Cell]): Seq[Cell] = {
    for (line <- current.x - 1 to current.x + 1;
         column <- current.y - 1 to current.y + 1;
         if line != current.x || column != current.y
    )
      yield liveCells
        .find(f => f.x == line && f.y == column)
        .getOrElse(Cell(line, column,Dead))
  }
}
