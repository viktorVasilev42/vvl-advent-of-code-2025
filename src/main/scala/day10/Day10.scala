package day10

type Button = Set[Int]

type AugMatrix = Array[Array[Double]]
type AugMatrixWithMaxJoltage = (AugMatrix, Int)


object Day10:
  private val inputFilename = "input.txt"

  def firstDepthThatSatisfies(p: Int => Boolean): Int =
    Iterator.iterate(0)(_ + 1).dropWhile(!p(_)).next()

  private def augmentedMatricesFromMachines(machines: List[Machine]): List[AugMatrixWithMaxJoltage] =
    machines.map(mach => {
      val matrix = mach.joltages.indices
        .map(idx => {
          val coef = mach.buttons.indices.toArray
            .map(buttonIdx => if mach.buttons(buttonIdx).contains(idx) then 1.0 else 0.0)
          val rhs = mach.joltages(idx).toDouble
          coef :+ rhs
        })
        .toArray
      val maxJoltage = mach.joltages.max
      (matrix, maxJoltage)
    })

  private def getFreeVarsAndComputeFunction(aug: AugMatrix): (Seq[Int], Array[Double] => Array[Double]) =
    val ps = ParametricSolution.extractSolution(LinSys.rref(LinSys(aug)))
    (ps.freeVars, ps.compute)

  private def minButtonPressesForMachines(machines: List[Machine]): List[Long] =
    augmentedMatricesFromMachines(machines)
      .map((aug, maxJoltage) => {
        val (freeVars, compute) = getFreeVarsAndComputeFunction(aug)
        val values = (0 to maxJoltage).map(_.toDouble)
        val freeVarRanges = Seq.fill(freeVars.size)(values)

        ParametricSolution.cartesian(freeVarRanges)
          .map(params => compute(params.toArray))
          .map(_.map(d => (d, math.round(d))))
          .filter(a => a.forall((d, l) => math.abs(d - l) < 1e-6) && a.forall((_, l) => l >= 0L))
          .map(_.map((_, l) => l).sum)
          .min
      })

  def part1(): Int =
    val machines = Machine.readMachinesFromFile(inputFilename)

    machines.map(mach => {
      val (onLights, offLights) = mach.indicesOfOnAndOffLights()
      val startMap = (onLights ++ offLights).map(i => (i -> 0)).toMap

      firstDepthThatSatisfies(depth => {
        def tryAll(buttons: List[Button], acc: Map[Int, Int], currDepth: Int,
                   onLights: List[Int], offLights: List[Int]): Boolean = {
          if (currDepth == 0) {
            onLights.forall(acc.getOrElse(_, 0) % 2 != 0)
              && offLights.forall(acc.getOrElse(_, 0) % 2 == 0)
          } else
            buttons.exists(b => {
              val newAcc = acc.map { case (k, v) =>
                if (b.contains(k)) k -> (v + 1)
                else k -> v
              }
              tryAll(buttons, newAcc, currDepth - 1, onLights, offLights)
            })
        }

        tryAll(mach.buttons, startMap, depth, onLights, offLights)
      })
    }).sum

  def part2(): Long =
    val machines = Machine.readMachinesFromFile(inputFilename)
    minButtonPressesForMachines(machines).sum


