import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: proximo
 * Date: 4/16/13
 * Time: 11:35 PM
 */
def uniqueNumbers(min: Int, max: Int): Seq[Int] = {
  require(min <= max)

  @tailrec
  def isUniqueRec(bits: Int)(number: Int): Boolean =
    if (number == 0) true
    else {
      val newBits = bits | (1 << number % 10)

      if (newBits == bits) false
      else isUniqueRec(newBits)(number / 10)
    }

  def unique = isUniqueRec(0)_

  (min to max).filter(unique(_))
}

uniqueNumbers(50, 103)










































