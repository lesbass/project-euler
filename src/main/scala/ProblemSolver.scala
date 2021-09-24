import scala.language.postfixOps

object ProblemSolver {

  def problem1(n: Int): Int =
    (0 until n).reduce((sum, a) =>
      sum + (if (a % 3 == 0 || a % 5 == 0) a else 0)
    )

  def problem2(n: Int): Int = {

    def run(limit: Int)(prevNumber: Int, currNumber: Int): Int = {
      def checkEven(n: Int) = if (n % 2 == 0) n else 0

      def runInside(sum: Int) =
        checkEven(sum) + runWithLimit(currNumber, sum)

      if (currNumber > limit) 0 else runInside(prevNumber + currNumber)
    }

    def runWithLimit = run(n) _

    runWithLimit(1, 1)
  }

  def problem5(n: Int): Int = {
    def checkEvenlyDivisible(limit: Int)(n: Int) =
      !(1 to limit).exists(n == 0 || n % _ != 0)

    def checkEvenlyDivisibleWithLimit = checkEvenlyDivisible(n) _

    (0 to Int.MaxValue by n).find(checkEvenlyDivisibleWithLimit).getOrElse(-1)
  }

  def problem6(n: Int): Long = {
    def sumOfSquares(n: Int) = (1 to n).map(Math.pow(_, 2)).sum.toLong
    def squareOfSum(n: Int) = Math.pow((1 to n).sum, 2).toLong
    squareOfSum(n) - sumOfSquares(n)
  }

  def problem21(n: Int): Int = {
    def isAmicable(n: Int) = {
      def getSumOfDivisors(n: Int) = (1 to n / 2).filter(n % _ == 0).sum
      List(getSumOfDivisors(n))
        .filter(_ != n)
        .exists(x => n == getSumOfDivisors(x))
    }

    (1 to n).filter(isAmicable).sum
  }
}
