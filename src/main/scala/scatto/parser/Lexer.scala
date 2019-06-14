package scatto.parser

import scatto.parser.types.Parser
import scatto.parser.MonadInstances._
import scatto.parser.MonadPlusInstances._

object Lexer {
  def space: Parser[String] =
    for {
      s <- Combinators.many(char(' '))
    } yield s.mkString

  def space1: Parser[String] =
    for {
      s <- Combinators.many1(char(' '))
    } yield s.mkString

  def ident: Parser[String] =
    for {
      x <- Combinators.lower <|> char('_')
      xs <- Combinators.many(Combinators.alphanum <|> char('_'))
    } yield (x +: xs).mkString

  def nat: Parser[Int] = {
    def toNumber(xs: List[Char]): Int =
      xs.map(x => x.toInt - '0'.toInt).reduceLeft((acc, x) => 10 * acc + x)

    for {
      xs <- Combinators.many1(Combinators.digit)
    } yield toNumber(xs)
  }

  def int: Parser[Int] =
    (for {
      _ <- char('-')
      n <- nat
    } yield -n) <|> nat

  def double: Parser[Double] =
    for {
      whole <- int
      _ <- char('.')
      dec <- nat
    } yield s"$whole.$dec".toDouble

  def char(c: Char): Parser[Char] = Combinators.satisfy(y => c == y)

  def string(s: String): Parser[String] = {
    if (s.isEmpty) Monad[Parser].unit("")
    else
      for {
        x <- char(s.head)
        xs <- string(s.tail)
      } yield x +: xs
  }

  def array[A](pa: Parser[A]): Parser[List[A]] =
    bracket(char('['), Combinators.sepBy1(pa, char(',')), char(']'))

  def bracket[A](
    open: Parser[Char],
    pa: Parser[A],
    close: Parser[Char]): Parser[A] =
    for {
      _ <- open
      x <- pa
      _ <- close
    } yield x
}
