package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    //println(product(x=> x*x)(2,3))

    println(fixedPoint(x=>1 + x/2)(2.0))
  }



  /**
   * Exercise 1
   */
  def pascal(c: Int, r: => Int): Int =
    if (c == r || c == 0) 1
    else {
      return pascal(c - 1, r - 1) + pascal(c, r - 1)
    }


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {


    def balanceCount(chars: List[Char], left: Int, right: Int): Boolean = {

      if (left < right) return false

      if (!chars.isEmpty) {
        if (chars.head == '(') balanceCount(chars.tail, left + 1, right)
        else if (chars.head == ')') balanceCount(chars.tail, left, right + 1)
        else balanceCount(chars.tail, left, right)
      }
      else
        return left == right

    }


    if (!chars.isEmpty)
      balanceCount(chars, 0, 0)
    else
      true


  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty && money > 0) return 0
    if (money == 0) return 1
    if (money < 0) return 0
    val x = countChange(money, coins.tail)
    val y = countChange(money - coins.head, coins)
    return x + y

  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def fact(n: Int): Int = product(x => x * x)(1, n)

  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double): Boolean = {
    abs((x - y) / x) / x < tolerance
  }

  def abs(x: Double): Double = {
    if (x < 0.0) -1 * x else x
  }

  def fixedPoint(f: Double => Double)(n: Double) = {
    def iter(myguess: Double): Double = {
      val mynewguess = f(myguess)
      if (isCloseEnough(myguess, mynewguess)) mynewguess
      else
        iter(mynewguess)
    }
    iter(n)
  }

}
