/** 上位者問題
  *
  * G E N E R A T I N G
  *
  * 前提 A < B < C ... < Z
  *
  * 先頭のGの上位者はN, R, A, T, I, Nの5つ 次のEの上位者はN, R, T, I, N, Gの6つ
  */
object Surpasser {

  def msc(xs: List[Char]): Int = tails(xs).map {
    case z :: zs => scount(z, zs)
    case _       => 0
  }.max

  def scount(x: Char, xs: List[Char]): Int = xs.filter(_ > x).size

  def tails(xs: List[Char]): List[List[Char]] = xs match {
    case Nil     => Nil
    case x :: xs => (x :: xs) :: tails(xs)
  }
}
