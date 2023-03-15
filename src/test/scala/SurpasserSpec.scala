import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import Surpasser._

class SurpasserSpec extends AnyFlatSpec with Matchers {

  "tails" should "generate all tails of a list" in {
    tails(List(1, 2, 3)) should be(List(List(1, 2, 3), List(2, 3), List(3)))
  }

  "msc" should "return the max surpasser count" in {
    msc(List('G', 'E', 'N', 'E', 'R', 'A', 'T', 'I', 'N', 'G')) should be(6)

    // N E R A T I N G
    // 5
    msc(List('N', 'E', 'R', 'A', 'T', 'I', 'N', 'G')) should be(5)
  }
}
