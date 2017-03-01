object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    def loop(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else loop(n-1, n*acc)
    loop(n, 1)
  }

  def nthFib(n: Int): Int = {
    if (n == 0) 0
    else if (n < 2) 1
    else nthFib(n-1) + nthFib(n-2)
  }

  def findFirst(ss: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n+1)
    }
    loop(0)
  }

  def findFirst2[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)
    }
    loop(0)
  }

  def lessThan(x: Int, y: Int): Boolean = x < y

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n == as.length-1) true
      else if (!ordered(as(n), as(n+1))) false
      else loop(n+1)
    }
    loop(0)
  }

  private def formatAbs(n: Int) = {
    val msg = "The absolute value of %d is %d."
    msg.format(n, abs(n))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d if %d."
    msg.format(n, factorial(n))
  }

  private def formatNthFib(n: Int) = {
    val msg = "Item %d in the Fibonacci sequence is %d."
    msg.format(n, nthFib(n))
  }


  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}
