import scalaz.Scalaz._

import scala.language.postfixOps

object ProblemSolver {

  def problem1(n: Int): Int =
    (0 until n).reduce((sum, a) =>
      sum + (if (a % 3 == 0 || a % 5 == 0) a else 0)
    )

  def problem2(n: Int): Int = {
    implicit val limit: Int = n

    def run(prevNumber: Int, currNumber: Int)(implicit limit: Int): Int = {
      def checkEven(n: Int) = if (n % 2 == 0) n else 0

      def runInside(sum: Int) =
        checkEven(sum) + run(currNumber, sum)

      if (currNumber > limit) 0 else runInside(prevNumber + currNumber)
    }

    run(1, 1)
  }

  def problem5(n: Int): Int = {
    implicit val limit: Int = n

    def checkEvenlyDivisible(n: Int)(implicit limit: Int) =
      !(1 to limit).exists(n == 0 || n % _ != 0)

    (0 to Int.MaxValue by n).find(checkEvenlyDivisible).getOrElse(-1)
  }

  def problem6(n: Int): Long = {
    def sumOfSquares(n: Int) = (1 to n).map(Math.pow(_, 2)).sum.toLong
    def squareOfSum(n: Int) = Math.pow((1 to n).sum, 2).toLong
    squareOfSum(n) - sumOfSquares(n)
  }

  def problem21(n: Int): Int = {
    def isAmicable(n: Int) = {
      def getSumOfDivisors(n: Int) = (1 to n / 2).filter(n % _ == 0).sum
      def notTheSame(x: Int) =
        if (x != n) { Some(x) }
        else { None }
      def checkEqualsSumOfDivisorsDivisors(x: Option[Int]) =
        x match {
          case Some(a) => n == (a |> getSumOfDivisors)
          case _       => false
        }

      n |> getSumOfDivisors |> notTheSame |> checkEqualsSumOfDivisorsDivisors
    }

    (1 to n).filter(isAmicable).sum
  }
}
