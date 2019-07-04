package scatto

import scatto.parser.MonadInstances._
import scatto.parser.types._
import scatto.parser._

case class Letters(s: String)

object Scatto extends App {
  def twoLetterParser(p: Parser[Char]): Parser[Letters] = {
    for {
      x <- p
      y <- p
    } yield Letters(s"$x$y")
  }

  val c = Combinators

  println(parse(twoLetterParser(c.letter), "ivan"))
}
