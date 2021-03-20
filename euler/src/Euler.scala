package euler

import scala.annotation

object Euler {

  /** Solution to problem 11
    */
  object MaxGridProduct {

    type Grid = Vector[Vector[Int]]
    def parseGrid(g: String): Grid =
      g.split("\n").map(_.split(" ").map(_.toInt).toVector).toVector

    def transpose(g: Grid): Grid = (0 to g.head.length - 1)
      .map(c => (0 to g.length - 1).map(r => g(r)(c)).toVector)
      .toVector

    def rotate(g: Grid): Grid = (0 to g.length - 1)
      .map(r =>
        (0 to g.head.length - 1).map(c => g(c)(g.head.length - 1 - r)).toVector
      )
      .toVector

    def getLeftToRightDiagonals(g: Grid): Vector[Vector[Int]] =
      (0 to g.length - 1)
        .map(r =>
          (0 to g.head.length - 1 - r)
            .map(c => (g(r + c)(c), g(c)(r + c)))
            .unzip
        )
        .flatMap { case ((l1, l2)) =>
          List(l1.toVector, l2.toVector)
        }
        .toVector

    def getDiagonals(g: Grid): Grid = getLeftToRightDiagonals(
      g
    ) :++ (getLeftToRightDiagonals _).compose(rotate)(g)

    def getWindowedSums(g: Grid, w: Int): List[Int] =
      g.flatMap(_.sliding(w).filter(_.length == w).map(_.product)).toList

    def getHorizontalSums(g: Grid, w: Int): List[Int] = getWindowedSums(g, w)

    def getVerticalSums(g: Grid, w: Int): List[Int] =
      (getWindowedSums(_, w)).compose(transpose)(g)

    def getDiagonalSums(g: Grid, w: Int): List[Int] =
      (getWindowedSums(_, w)).compose(getDiagonals)(g)

    def getMaxWindowSum(g: Grid, w: Int): Int = List(
      getHorizontalSums(_, _),
      getVerticalSums(_, _),
      getDiagonalSums(_, _)
    ).map(f => f(g, w).max).max
  }

  /** Solution to problem 17
    */
  object NumberLetterCounts {
    val numMap = Map(
      0 -> "",
      1 -> "one",
      2 -> "two",
      3 -> "three",
      4 -> "four",
      5 -> "five",
      6 -> "six",
      7 -> "seven",
      8 -> "eight",
      9 -> "nine",
      10 -> "ten",
      11 -> "eleven",
      12 -> "twelve",
      13 -> "thirteen",
      14 -> "fourteen",
      15 -> "fifteen",
      16 -> "sixteen",
      17 -> "seventeen",
      18 -> "eighteen",
      19 -> "nineteen",
      20 -> "twenty",
      30 -> "thirty",
      40 -> "forty",
      50 -> "fifty",
      60 -> "sixty",
      70 -> "seventy",
      80 -> "eighty",
      90 -> "ninety"
    )

    val powerMap = Map(
      0 -> "",
      1 -> "",
      2 -> "hundred",
      3 -> "thousand"
    )

    //   @annotation.tailrec
    def getDigits(i: Int): List[Int] =
      if (i == 0) List.empty[Int] else i % 10 :: getDigits(i / 10)

    def digitsToNumber(ds: Iterable[Int]): Int = ds
      .foldLeft((0, 0)) { case ((power, number), digit) =>
        (power + 1, digit * math.pow(10, power).toInt + number)
      }
      ._2

    def handleHundreds(hundreds: Int): String =
      if (hundreds > 0) numMap(hundreds) + " hundred" else ""

    def handleTens(
        hundreds: Int,
        tens: Int,
        ones: Int,
        power: Int,
        maxPower: Int
    ) =
      (if (maxPower >= 3 && power == 0 && digitsToNumber(List(ones, tens)) > 0)
         "and "
       else "") + numMap.getOrElse(
        digitsToNumber(List(ones, tens)),
        List(numMap(tens * 10), numMap(ones)).filter(_.length > 0).mkString(" ")
      )

    def tripletToWords(triplet: List[Int], power: Int, maxPower: Int) =
      triplet match {
        case ones :: tens :: hundreds :: Nil =>
          List(
            handleHundreds(hundreds),
            handleTens(hundreds, tens, ones, power, maxPower)
          ).filter(!_.isEmpty).mkString(" ")
        case ones :: tens :: Nil =>
          handleTens(0, tens, ones, power, maxPower)
        case ones :: Nil =>
          handleTens(0, 0, ones, power, maxPower)
        case _ => ""
      }

    def numberToWords(i: Int): String = getDigits(i)
      .sliding(3, 3)
      .foldLeft((getDigits(i).length, 0, "")) {
        case ((maxPower, power, words), triplet) =>
          (
            maxPower,
            power + 3,
            List(
              tripletToWords(triplet, power, maxPower),
              powerMap(power),
              words
            )
              .filter(!_.isEmpty)
              .mkString(" ")
          )
      }
      ._3
  }
}
