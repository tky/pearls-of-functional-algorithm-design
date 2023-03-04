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
}
