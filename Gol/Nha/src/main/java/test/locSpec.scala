package test

/**
 * Created by ducnhabui on 10/23/15.
 */

import bdn.gol.loc
import org.scalatest._


class IocSpec extends FlatSpec with Matchers {
  "Gol" should "Any live cell with fewer than two live neighbours dies, as if caused by under-population" in {
      val loc = new loc()
    loc.solve()
  }
}


