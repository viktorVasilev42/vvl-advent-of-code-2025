package day10

case class LinSys(a: Array[Array[Double]]) {

  val m: Int = a.length
  val n: Int = a(0).length - 1

  def copy(): LinSys =
    LinSys(a.map(_.clone()))
}

object LinSys:
  def rref(sys: LinSys, eps: Double = 1e-9): (LinSys, Array[Int]) = {
    val m = sys.m
    val n = sys.n
    val a = sys.copy().a

    var row = 0
    val pivotCol = Array.fill(m)(-1)

    for (col <- 0 until n if row < m) {
      var best = row
      var bestVal = math.abs(a(row)(col))
      var r = row + 1
      while (r < m) {
        val v = math.abs(a(r)(col))
        if (v > bestVal) {
          bestVal = v; best = r
        }
        r += 1
      }

      if (bestVal > eps) {
        val tmp = a(row);
        a(row) = a(best);
        a(best) = tmp

        val pivot = a(row)(col)
        var j = col
        while (j <= n) {
          a(row)(j) /= pivot
          j += 1
        }
        pivotCol(row) = col

        var i = 0
        while (i < m) {
          if (i != row) {
            val factor = a(i)(col)
            if (math.abs(factor) > eps) {
              j = col
              while (j <= n) {
                a(i)(j) -= factor * a(row)(j)
                j += 1
              }
            }
          }
          i += 1
        }

        row += 1
      }
    }

    (LinSys(a), pivotCol)
  }


case class ParametricSolution(freeVars: Seq[Int], compute: Array[Double] => Array[Double])

object ParametricSolution:
  def extractSolution(rref: (LinSys, Array[Int]), eps: Double = 1e-9): ParametricSolution = {
    val (rSys, pivotCol) = rref
    val a = rSys.a
    val m = rSys.m
    val n = rSys.n

    val isPivot = Array.fill(n)(false)
    for (r <- 0 until m if pivotCol(r) >= 0) isPivot(pivotCol(r)) = true

    val free = (0 until n).filter(j => !isPivot(j))

    case class PivotExpr(pivotCol: Int, rhs: Double, coeff: Map[Int, Double])

    val pivotExprs = (0 until m).flatMap { r =>
      val pc = pivotCol(r)
      if (pc < 0) None
      else {
        val rhs = a(r)(n)
        val mp = free.iterator
          .map(j => j -> a(r)(j))
          .filter { case (_, c) => math.abs(c) > eps }
          .toMap
        Some(PivotExpr(pc, rhs, mp))
      }
    }

    def compute(params: Array[Double]): Array[Double] = {
      require(params.length == free.length)
      val x = Array.fill[Double](n)(0.0)

      free.zipWithIndex.foreach { case (j, k) => x(j) = params(k) }

      pivotExprs.foreach { pe =>
        var v = pe.rhs
        pe.coeff.foreach { case (j, c) =>
          v -= c * x(j)
        }
        x(pe.pivotCol) = v
      }
      x
    }

    ParametricSolution(free, compute)
  }

  def cartesian[T](xss: Seq[Seq[T]]): Iterator[Seq[T]] =
    xss.foldLeft(Iterator(List.empty[T]): Iterator[List[T]]) { (acc, xs) =>
      for {
        a <- acc
        x <- xs.iterator
      } yield x :: a
    }
