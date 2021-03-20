import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import euler.Euler.NumberLetterCounts._
import euler.Euler.MaxGridProduct._

class NumberLetterCountsSpec extends AnyFlatSpec with should.Matchers {
  "getDigits" should "return list of decimal digits in a number in little-endian order" in {
    getDigits(1234) should be(List(4, 3, 2, 1))
  }

  it should "handle number with trailing zeros" in {
    getDigits(543210) should be(List(0, 1, 2, 3, 4, 5))
  }

  it should "return empty list for zero" in {
    getDigits(0) should be(List.empty[Int])
  }

  "digitsToNumber" should "encode little-endian digit list as integer" in {
    digitsToNumber(List(1, 2, 3)) should be(321)
  }

  it should "handle a list containing 0" in {
    digitsToNumber(List(1, 0, 2)) should be(201)
  }

  it should "handle leading 0" in {
    digitsToNumber(List(0, 0, 0, 1)) should be(1000)
  }

  it should "handle trailing 0" in {
    digitsToNumber(List(2, 1, 0)) should be(12)
  }

  "numberToWords" should "handle number less than ten" in {
    numberToWords(5) should be("five")
  }

  it should "handle number between 9 and 20 exclusive" in {
    numberToWords(10) should be("ten")
    numberToWords(16) should be("sixteen")
  }

  it should "handle number between 19 and 100 exclusive ending in zero" in {
    numberToWords(20) should be("twenty")
  }

  it should "handle number between 19 and 100 exclusive ending in non-zero" in {
    numberToWords(42) should be("forty two")
  }

  it should "handle number between 99 and 1000 exclusive ending in non-zero" in {
    numberToWords(101) should be("one hundred and one")
    numberToWords(342) should be("three hundred and forty two")
    numberToWords(500) should be("five hundred")
  }

  it should "handle number between 999 and 1e6 exclusive" in {
    numberToWords(1000) should be("one thousand")
    numberToWords(1001) should be("one thousand and one")
    numberToWords(5432) should be("five thousand four hundred and thirty two")
    numberToWords(51032) should be("fifty one thousand and thirty two")
    numberToWords(481790) should be(
      "four hundred eighty one thousand seven hundred and ninety"
    )
  }

  // it should "let us count letters in words" in {
  //   numberToWords(5432).filter(_ != ' ').length should be(35)
  //   (1 to 1000).map(numberToWords(_).replace(" ", "").length).sum should be(12312412)
  // }

}

class MaxGridProductSpec extends AnyFlatSpec with should.Matchers {

  "transpose" should "transpose a nested vector" in {
    transpose(Vector(Vector(1, 2), Vector(3, 4))) should be(
      Vector(Vector(1, 3), Vector(2, 4))
    )
  }

  "getDiagonals" should "return a list of all diagonals" in {
    val l = Vector(Vector(1, 2, 3), Vector(4, 5, 6), Vector(7, 8, 9))
    // println(rotate(l).map(_.mkString(" ")).mkString("\n") + "\n")
    println(getDiagonals(l).map(_.mkString(" ")).mkString("\n") + "\n")
    // println(getDiagonals(transpose(l)).map(_.mkString(" ")).mkString("\n"))
    // getDiagonals(l) should be(Vector())
  }
}
