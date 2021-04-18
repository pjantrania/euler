package regex

sealed trait State {
  def process(in: Char): State
  protected def nextState(n: State) = n match {
    case pks: PartialKleeneStar => pks.resolve(this)
    case _                      => n
  }
}

object State {
  def apply(symbol: Char, next: State) = symbol match {
    case '.' => Wildcard(next)
    case '*' => PartialKleeneStar(next)
    case _ if next.isInstanceOf[PartialKleeneStar] =>
      next.asInstanceOf[PartialKleeneStar].resolve(Character(symbol, Empty))
    case _ => Character(symbol, next)
  }
}

sealed trait TerminalState extends State {
  def process(in: Char): State = this
}

case object Failure extends TerminalState

case object Success extends TerminalState

case object Empty extends TerminalState

case class Character(c: Char, next: State) extends State {
  def process(in: Char): State = if (in == c) nextState(next) else Failure
}

case class Wildcard(next: State) extends State {
  def process(in: Char): State = nextState(next)
}

case class KleeneStar(inner: State, next: State) extends State {
  def process(in: Char): State = inner.process(in) match {
    case Failure => next.process(in)
    case _       => this
  }
}

case class PartialKleeneStar(next: State) extends State {
  def process(in: Char): State = Failure
  def resolve(e: State): State = KleeneStar(e, next)
}

sealed trait Regex {
  def isMatch(in: String): Boolean
}

private case class RegexImpl(ex: State) extends Regex {
  def isMatch(in: String): Boolean =
    in.foldLeft(ex)((e, c) => e.process(c)) == Success
}

object Regex {
  def apply(expression: String): Regex = RegexImpl(
    expression.foldRight(Success: State)(State(_, _))
  )

  def isMatch(expression: String, input: String): Boolean =
    Regex(expression).isMatch(input)
}
