import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: proximo
 * Date: 4/16/13
 * Time: 11:35 PM
 */
def uniqueNumbers(min: Long, max: Long): Seq[Long] = {
  require(min <= max)

  @tailrec
  def isUniqueRec(bits: Int)(number: Long): Boolean =
    if (number == 0) true
    else {
      val newBits = bits | (1 << number % 10)

      if (newBits == bits) false
      else isUniqueRec(newBits)(number / 10)
    }

  def unique = isUniqueRec(0)_

  def stream(i: Long, max: Long): Stream[Long] =
    if (i < max) i #:: stream(i + 1, max)
    else Stream[Long]()
  stream(min, max).filter(unique(_))
  //(min to max).filter(unique(_))
}
uniqueNumbers(0, 9876543210l).foreach((l: Long) => {})


//uniqueNumbers(0, 1000).toList
















































