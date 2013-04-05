package recfun
import common._

object Main {
  def main(args: Array[String]) {
    val res = countChange(4, List(1, 2))

    println(res)
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r)
      1
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balance(opened: Int, chars: List[Char]): Boolean = {
      if (opened < 0)
        false
      else if (chars.isEmpty)
        opened == 0
      else {
        val next = chars.head match {
          case '(' => opened + 1
          case ')' => opened - 1
          case _ => opened
        }

        balance(next, chars.tail)
      }
    }

    balance(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(sum: Int, coins: List[Int]): Int = {
      if (sum == money)
        1
      else if (sum > money || coins.isEmpty)
        0
      else {
        count(sum + coins.head, coins) + 
        count(sum, coins.tail)
      }
    }

    count(0, coins)
  }
}
