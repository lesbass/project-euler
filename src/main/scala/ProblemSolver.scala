import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.runtime.Nothing$

object ProblemSolver {

  def problem1(n: Int): Int =
    (0 until n).reduce((sum, a) =>
      sum + (if (a % 3 == 0 || a % 5 == 0) a else 0)
    )

  def problem2(n: Int): Int = {

    def run(limit: Int)(prevNumber: Int, currNumber: Int): Int = {
      def checkEven(n: Int): Int = if (n % 2 == 0) n else 0

      def runInside(sum: Int): Int =
        checkEven(sum) + runWithLimit(currNumber, sum)

      if (currNumber > limit) 0 else runInside(prevNumber + currNumber)
    }

    def runWithLimit = run(n) _

    runWithLimit(1, 1)
  }

  def problem5(n: Int): Int = {
    def checkEvenlyDivisible(n: Int, limit: Int): Boolean =
      !(1 to limit).exists(n == 0 || n % _ != 0)

    (0 to Int.MaxValue by n).find(checkEvenlyDivisible(_, n)).getOrElse(-1)
  }
}
