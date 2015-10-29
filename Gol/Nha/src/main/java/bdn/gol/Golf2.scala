package bdn.golf

class Golf2 {
  val defFunc = (value:Int,line:Int,column:Int)=> value

  def tick(matrix: Seq[Seq[Int]]): Seq[Seq[Int]] =
      getNeighbours(matrix,matrix.length,func=
        (cell,line,column)=> evolve(cell, getNeighbours(matrix,1,line,column).flatten.sum - cell))

    def getNeighbours(matrix:Seq[Seq[Int]],distance:Int  = 1,line:Int = 0 ,column:Int = 0 ,func:(Int,Int,Int)=>Int = defFunc):Seq[Seq[Int]]=
      for (i <- Math.max(line - distance, 0) to Math.min(line + distance, matrix.length - 1))
        yield for(j <- Math.max(column - distance, 0) to Math.min(column + distance, matrix.length -1))
          yield func(matrix(i)(j),i,j)

  def evolve(cell:  Int, aliveNeighbours:Int):  Int =
    cell match {
      case 0 if aliveNeighbours == 3 => 1
      case 0 => 0
      case _ => aliveNeighbours match {
        case 2 | 3 => 1
        case _ => 0
      }
    }

}
