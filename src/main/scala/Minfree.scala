import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

object Minfree {
  def minfree(xs: Array[Int]): Int = {
    Stream.from(0).find(!xs.contains(_)).get
  }

  def search(xs: Array[Boolean]): Int = xs.takeWhile(identity).length

  def accumArray[A: ClassTag](
      op: (A, A) => A,
      init: A,
      bounds: (Int, Int),
      elems: Seq[(Int, A)]
  ): Array[A] = {
    val len = bounds._2 - bounds._1 + 1
    val arr = new ArrayBuffer[A](len)
    arr ++= Seq.fill(len)(init)

    for ((i, x) <- elems) {
      val index = i - bounds._1
      arr(index) = op(arr(index), x)
    }
    arr.toArray
  }

  def checklist(xs: Array[Int]): Array[Boolean] = accumArray(
    (x: Boolean, y: Boolean) => x || y,
    false,
    (0, xs.length),
    xs.map(x => (x, true))
  )

  /*
   * 分割統治 (as ++ bs) \\ cs = (as \\ cs) ++ (bs \\ cs) - 1 as \\ (bs ++ cs) =
   * (as \\ bs) \\ cs - 2 (as \\ bs) \\ cs = (as \\ cs) \\ bs -3
   *
   * asとvsが互いに素 => as \\ vs = as bsとusが互いに素 => bs \\ us = bs の場合 (as ++ bs) \\
   * (vs ++ us) = (as \\ (vs ++ us)) ++ (bs \\ (vs ++ us)) (1より) \= ((as \\ vs)
   * \\ us) ++ ((bs \\ vs) \\ us) (2より) \= ((as \\ vs) \\ us) ++ ((bs \\ us) \\
   * vs) (3より) asとvs、bsとusは互いに素なので as \\ vs = as, bs \\ us = bs \= (as \\ us)
   * ++ (bs \\ vs)
   *
   * より
   * 任意の自然数bに対して
   * [0..] \\ xs = ([0..b-1] \\ us) ++ ([b..] \\ vs)
   * が成立する
   * ただし
   * us = xs.filter(_ < b)
   * vs = xs.filter(_ >= b)
   *
   * minfree xs = if null([0..b-1] \\ us)
   * then head([b..] \\ vs)                // bより小さい値の差集合が存在しない => xsにはb以下の値が全て存在する =>bより大きい差集合の先頭が求める値
   * else minfree head([0..] \\ us)        // bより小さい値の差集合が存在する => 差集合の先頭が求める値
   *
   *
   * usは重複のないb未満の自然数の集合なので、
   * null([0..b-1] \\ us) === length us == b
   *
   * ex
   * b = 3, us = [0, 1, 2]
   * null([0..3] \\ [0, 1, 2]) === length [0, 1, 2] == 3 === true
   *
   * b = 3, us = [0, 2]
   * null([0..3] \\ [0, 2]) === length [0, 2] == 3 === false
   *
   * よって時間がかかるnull([0..b-1] \\ us)はlength us == bと同値
   */
  def minfree2(xs: Array[Int]): Int = minfrom(0, xs.length, xs)

  def minfrom(a: Int, n: Int, xs: Array[Int]): Int = {
    if (n == 0) a
    else {
      val b = a + 1 + (n / 2)
      val (us, vs) = xs.partition(_ < b)
      val m = us.length

      // m == b - a => usにはbより小さい値の差集合が存在しない
      // vsに求める値が存在する
      if (m == b - a)
        minfrom(b, n - m, vs)
      else minfrom(a, m, us)
    }
  }
}
