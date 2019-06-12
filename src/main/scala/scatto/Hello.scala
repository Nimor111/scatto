package scatto

import scatto.parser.MonadInstances._
import scatto.parser.types._
import scatto.parser._

case class Letters(s: String)

object Hello extends App {
  def parse[A](p: Parser[A], s: String): Unit = {
    val twoLettersParser = for {
      x <- p
      y <- p
    } yield Letters(s"$x$y")

    println(twoLettersParser(s))
  }

  val c = new Combinators

  parse(c.letter, "ivan")
}
