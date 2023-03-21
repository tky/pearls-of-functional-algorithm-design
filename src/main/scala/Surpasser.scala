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

  def table(xs: List[Char]): List[(Char, Int)] = xs match {
    case List(x) => List((x, 0))
    case xs => {
      val m = xs.size
      val n = m / 2
      val (ys, zs) = xs.splitAt(n)
      join(m - n, table(ys), table(zs))
    }
  }

  def join(
      n: Int,
      txs: List[(Char, Int)],
      tys: List[(Char, Int)]
  ): List[(Char, Int)] =
    (n, txs, tys) match {
      case (0, tsx, Nil) => txs
      case (n, Nil, tsy) => tys
      case (n, ((x, c) :: txs2), ((y, d) :: tys2)) =>
        if (x < y) (x, c + n) :: join(n, txs2, tys)
        else (y, d) :: join(n - 1, txs, tys2)
      case _ => Nil
    }

  def msc2(xs: List[Char]): Int = table(xs).map(_._2).max
}
