package regex

sealed trait Ex {
  def process(in: Char): Ex
  protected def nextState(n: Ex) = n match {
    case pks: PartialKleeneStar => pks.resolve(this)
    case _                      => n
  }
}

case object Failure extends Ex {
  def process(in: Char): Ex = Failure
}

case object Success extends Ex {
  def process(in: Char): Ex = Success
}

case class Character(c: Char, next: Ex) extends Ex {
  def process(in: Char): Ex = if (in == c) nextState(next) else Failure
}

case class Wildcard(next: Ex) extends Ex {
  def process(in: Char): Ex = nextState(next)
}

case class KleeneStar(inner: Ex, next: Ex) extends Ex {
  def process(in: Char): Ex = inner.process(in) match {
    case Failure => next.process(in)
    case _       => this
  }
}

case class PartialKleeneStar(next: Ex) extends Ex {
  def process(in: Char): Ex = Failure
  def resolve(e: Ex): Ex = KleeneStar(e, next)
}

sealed trait Regex {
  def isMatch(in: String): Boolean
}

private case class RegexImpl(ex: Ex) extends Regex {
  def isMatch(in: String): Boolean =
    in.foldLeft(ex)((e, c) => e.process(c)) == Success
}

object Regex {
  def apply(expression: String): Regex = RegexImpl(
    expression.foldRight(Success.asInstanceOf[Ex])(processEx(_, _))
  )

  private def processEx(c: Char, e: Ex): Ex = c match {
    case '.' => Wildcard(e)
    case '*' => PartialKleeneStar(e)
    case _ if e.isInstanceOf[PartialKleeneStar] =>
      e.asInstanceOf[PartialKleeneStar].resolve(Character(c, Success))
    case _ => Character(c, e)
  }
}
