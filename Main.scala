package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Parentheses Balancing")
    val chars: List[Char] = ":-)".toList
    println(balance(chars))

    println("Counting Change")
    val coins = List(1, 5, 10, 20, 50)
    println(countChange(100, coins))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) false
    else {
      def help(chars: List[Char], numOfLeft: Int): Int = {
        if (chars.isEmpty) numOfLeft
        else if (chars.head == '(' && numOfLeft >= 0) help(chars.tail, numOfLeft + 1)
        else if (chars.head == ')') help(chars.tail, numOfLeft - 1)
        else help(chars.tail, numOfLeft)
      }
      help(chars, 0) == 0
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
