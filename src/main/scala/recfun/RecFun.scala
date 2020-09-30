package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def isBlance(chars: List[Char], acc: Int): Int = {
      if (chars.isEmpty || acc < 0) acc
      else {
        val char = chars.head
        isBlance(chars.tail, if (char == '(') acc + 1 else if (char == ')') acc - 1 else acc)
      }
    }
    isBlance(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countHelper(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countHelper(money - coins.head, coins) + countHelper(money, coins.tail)
    }
    countHelper(money, coins)
  }
}
