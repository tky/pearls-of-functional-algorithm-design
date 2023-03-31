/** 狭義単調増加関数とは 定義域内の任意の2つの異なる引数について、一方がもう一方よりも大きい場合には、対応する値も同様に大きくなるような関数のことです。
  * 例えば、以下のような関数は狭義単調増加関数です。 f(x) = x + 1：xが大きくなるにつれて、f(x)も大きくなる。 g(x) =
  * 2x：xが大きくなるにつれて、g(x)も大きくなる。 h(x, y) = x + y：xまたはyの少なくとも一方が大きくなる場合、h(x,
  * y)も大きくなる。
  *
  * 一方、以下のような関数は狭義単調増加関数ではありません。 f(x) = x^2：xが大きくなってもf(x)が小さくなる場合がある。(ex, x1 =
  * -2, x2 = -1、x1 < x2だがf(x1) > f(x2)) g(x) = -x：xが大きくなってもg(x)が小さくなる場合がある。 h(x,
  * y) = x * y：xとyが同時に大きくなる場合、h(x, y)が小さくなる場合がある。
  */

object SaddlebackSearch {

  /*
   * invert :: ((Int, Int) -> Int) -> Int -> [(Int, Int)]
   * invert f z = [(x, y) | x <- [0..z], y <- [0..z], f (x, y) == z]
   */
  def invert1(f: (Int, Int) => Int, z: Int): Seq[(Int, Int)] = {
    for {
      x <- 0 to z
      y <- 0 to z
      if f(x, y) == z
    } yield {
      (x, y)
    }
  }

  /*
   * invert :: ((Int, Int) -> Int) -> Int -> [(Int, Int)]
   * invert f z = [(x, y) | x <- [0..z], y <- [0..z-x], f (x, y) == z]
   *
   * x, y, z (f(x, y))は自然数(0以上の整数)
   * よってf(x, 0) >= x
   * もしf(x, 0) < xならばx = 0を代入すると
   * f(0, 0) < 0となりf(0, 0)が自然数でなくなってしまう。
   * (ex f(0, 0) = -1とすると、-1は自然数ではないので条件を満たさない)
   *
   * 同じ理由でf(0, y) >= y
   *
   * またfは単調増加関数なので
   * f(x, y) >= f(x, 0)
   * f(x, y) >= f(0, y)
   * よって
   * f(x, y) >= f(x, 0) + f(0, y) >= x + y
   * f(x, y) >= x + y
   * つまり
   * x + yがz(f(x,y))を超える範囲は捜索しなくて良い。
   * 捜索範囲はこの問題の場合は以下のように半分にできる。
   * x: 0 -> z
   * y: 0 -> z - x
   */
  def invert2(f: (Int, Int) => Int, z: Int): Seq[(Int, Int)] = {
    for {
      x <- 0 to z
      y <- 0 to z - x
      if f(x, y) == z
    } yield {
      (x, y)
    }
  }

  /** find(u, v) f z = [(x, y) | x <- [u..z], y <- [v, v -1..0], f (x, y) == z]
    */
  def find(u: Int, v: Int, f: (Int, Int) => Int, z: Int): Seq[(Int, Int)] = {
    for {
      x <- u to z
      y <- v to 0 by -1
      if f(x, y) == z
    } yield {
      (x, y)
    }
  }

  def invert3(f: (Int, Int) => Int, z: Int): Seq[(Int, Int)] = find(0, z, f, z)
}
