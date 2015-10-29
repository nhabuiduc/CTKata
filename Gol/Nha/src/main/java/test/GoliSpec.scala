package test

import bdn.golf.Golf2
import bdn.goli.{Cell, Goli}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by ducnhabui on 10/24/15.
 */
class GoliSpec extends FlatSpec with Matchers {
  "Gol" should "has the correct next generation" in {
    val goli = new Goli()

    val map = Seq(
                Cell(0,0)/*Dead*/,Cell(0,2),
                /*Dead*/Cell(1,1),/*Dead*/
                Cell(2,0)/*Dead*/,Cell(2,2))

     val result = goli.tick(map)

    val expected = Seq(
      /*Dead*/Cell(0,1),/*Dead*/
      Cell(1,0)/*Dead*/,Cell(1,2),
      /*Dead*/Cell(2,1)/*Dead*/)

    val resultSorted = result.sortBy(f=> f.x.toString + f.y.toString)

    assert(resultSorted==expected)

    val result1 = goli.tick(result)

    assert(resultSorted==expected)

  }


}


class GolfSpec extends FlatSpec with Matchers {
  "Gol" should "has the correct next generation" in {
    val golf = new Golf2()

    val map = Seq(
      Seq(1, 0, 1),
      Seq(0, 1, 0),
      Seq(1, 0, 1)
    )

    val result = golf.tick(map)

    assert(result.length==3)

    //    val map = Seq(
    //      Cell(0,0)/*Dead*/,Cell(0,2),
    //      /*Dead*/Cell(1,1),/*Dead*/
    //      Cell(2,0)/*Dead*/,Cell(2,2))
    //
    //    val result = goli.tick(map)
    //
    //    val expected = Seq(
    //      /*Dead*/Cell(0,1),/*Dead*/
    //      Cell(1,0)/*Dead*/,Cell(1,2),
    //      /*Dead*/Cell(2,1)/*Dead*/)
    //
    //    val resultSorted = result.sortBy(f=> f.x.toString + f.y.toString)
    //
    //    assert(resultSorted==expected)
    //
    //    val result1 = goli.tick(result)
    //
    //    assert(resultSorted==expected)
  }
}

