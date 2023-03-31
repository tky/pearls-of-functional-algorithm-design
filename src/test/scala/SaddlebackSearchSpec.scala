import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import SaddlebackSearch._

class SaddlebackSearchSpec extends AnyFlatSpec with Matchers {

  "invert" should "generate a list of pairs" in {
    val f = (x: Int, y: Int) => x + y
    val z = 3
    val expected = Seq((0, 3), (1, 2), (2, 1), (3, 0))
    invert1(f, z) should be(expected)
    invert2(f, z) should be(expected)
    invert3(f, z).sortBy(_._1) should be(expected)
  }
}
