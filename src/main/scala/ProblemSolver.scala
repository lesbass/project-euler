object ProblemSolver {
  def problem1(n: Int): String = {
    // https://projecteuler.net/problem=1
    var sum = 0
    for (a <- 1 until n) {
      if (a % 3 == 0 || a % 5 == 0) sum += a
    }
    sum.toString
  }
}
