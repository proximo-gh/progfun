package example

import scala.annotation.tailrec

/**
 * Created with IntelliJ IDEA.
 * User: proximo
 * Date: 4/22/13
 * Time: 11:05 PM
 */
object UniqueCiphers {
  def uniqueCiphers(min: Long, max: Long): Seq[Long] = {
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

  def main(args: Array[String]) {
    val unique = uniqueCiphers(0, 1000).toArray

    val deltas =
      for(i <- 1 until unique.size)
      yield unique(i) - unique(i - 1)

    println(deltas mkString ", ")
  }
}
