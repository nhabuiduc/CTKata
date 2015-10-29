package bdn.gol

case class Position(x: Int, y: Int, z: Int)

object Direction extends Enumeration {
  type Direction = Value
  val Left, Right, Top, Bottom, Up, Down = Value
}

class loc {

  import bdn.gol.Direction._

  val arr = Seq(3, 3, 3, 3, 2, 2, 2, 3, 2, 3, 3, 2, 3)
  val arr1 = Seq(2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 2)
  val arr2 = Seq(2, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1, 2, 1, 1, 2)

  var cubik =
    Seq(
      Seq(
        Seq(true, true, true),
        Seq(false, false, false),
        Seq(false, false, false)
      ),
      Seq(
        Seq(false, false, false),
        Seq(false, false, false),
        Seq(false, false, false)
      ),
      Seq(
        Seq(false, false, false),
        Seq(false, false, false),
        Seq(false, false, false)
      )
    )

  val list = scala.collection.mutable.ListBuffer[Direction]()

  def solve() = {
    list.clear()
    val result = move(Position(0, 0, 2), Up, 1, cubik)
    var sum = arr2.sum
  }

  def move(position: Position, direction: Direction, index: Int, cubik: Seq[Seq[Seq[Boolean]]]): Option[Seq[Seq[Seq[Boolean]]]] = {


    if (index >= arr2.length)
      return None

    val nextDirections: Traversable[Direction] = nextPosibleDirections(direction)

    for (d <- nextDirections) {

      val nextPosition = getNextPosition(d, arr2(index), position)

      if (inBound(nextPosition) && canMove(cubik, position, nextPosition)) {
        val nextCubik = move(cubik, position, nextPosition);

        if (isAllFilled(nextCubik)) return Option(nextCubik)

        val result = move(nextPosition, d, index + 1, nextCubik)
        result match {
          case None => None
          case _ => {
            list.append(d)
            return result
          }
        }
      }
    }

    return None
  }

  def countTrue(cubik: Seq[Seq[Seq[Boolean]]]): Int = {
    (for (
      x <- 0 to 2;
      y <- 0 to 2;
      z <- 0 to 2;
      if cubik(x)(y)(z)
    ) yield cubik(x)(y)(z)).length
  }

  def move(cubik: Seq[Seq[Seq[Boolean]]], curPosition: Position, position: Position): Seq[Seq[Seq[Boolean]]] = {

    val minX = Math.min(curPosition.x, position.x)
    val maxX = Math.max(curPosition.x, position.x)

    val minY = Math.min(curPosition.y, position.y)
    val maxY = Math.max(curPosition.y, position.y)

    val minZ = Math.min(curPosition.z, position.z)
    val maxZ = Math.max(curPosition.z, position.z)

    for (x <- 0 to 2)
      yield for (y <- 0 to 2)
        yield for (z <- 0 to 2)
          yield if (x >= minX && x <= maxX
            && y >= minY && y <= maxY
            && z >= minZ && z <= maxZ) {

            true
          } else cubik(x)(y)(z)


  }

  def canMove(cubik: Seq[Seq[Seq[Boolean]]], curPosition: Position, position: Position): Boolean = {
    val minX = Math.min(curPosition.x, position.x)
    val maxX = Math.max(curPosition.x, position.x)

    val minY = Math.min(curPosition.y, position.y)
    val maxY = Math.max(curPosition.y, position.y)

    val minZ = Math.min(curPosition.z, position.z)
    val maxZ = Math.max(curPosition.z, position.z)

    for (
      x <- minX to maxX;
      y <- minY to maxY;
      z <- minZ to maxZ;
      if (x != curPosition.x || y != curPosition.y || z != curPosition.z)
    ) if (cubik(x)(y)(z)) return false

    true
  }

  def isAllFilled(cubik: Seq[Seq[Seq[Boolean]]]): Boolean = {
    for (
      x <- 0 to 2;
      y <- 0 to 2;
      z <- 0 to 2;
      if (x != 1 || y != 1 || z != 1)
    ) if (!cubik(x)(y)(z)) return false

    return true
  }

  def inBound(position: Position): Boolean = {
    position.x >= 0 && position.x < 3 && position.y >= 0 && position.y < 3 && position.z >= 0 && position.z < 3
  }

  def getNextPosition(direction: Direction, step: Int, position: Position) = {
    direction match {
      case Left => Position(position.x - step, position.y, position.z)
      case Right => Position(position.x + step, position.y, position.z)
      case Top => Position(position.x, position.y + step, position.z)
      case Bottom => Position(position.x, position.y - step, position.z)
      case Up => Position(position.x, position.y, position.z + step)
      case Down => Position(position.x, position.y, position.z - step)
    }
  }

  def nextPosibleDirections(direction: Direction): Traversable[Direction] = {
    direction match {
      case Left | Right => Seq(Top, Bottom, Up, Down)
      case Top | Bottom => Seq(Left, Right, Up, Down)
      case Up | Down => Seq(Top, Bottom, Left, Right)
    }
  }
}




