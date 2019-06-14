package scatto.parser

import scatto.parser.types.Parser
import scatto.parser.MonadInstances._
import scatto.parser.MonadPlusInstances._

object Combinators {
  def item: Parser[Char] =
    input => if (input.isEmpty) List() else List((input.head, input.tail))
  def satisfy(pred: Char => Boolean): Parser[Char] =
    for {
      x <- item if pred(x)
    } yield x

  def char(c: Char): Parser[Char] = satisfy(y => c == y)
  def digit: Parser[Char] = satisfy(x => x >= '0' && x <= '9')
  def lower: Parser[Char] = satisfy(x => x >= 'a' && x <= 'z')
  def upper: Parser[Char] = satisfy(x => x >= 'A' && x <= 'Z')

  def letter: Parser[Char] = lower <|> upper
  def alphanum: Parser[Char] = letter <|> digit
  def word: Parser[String] = {
    val nonEmptyWord = for {
      x <- letter
      xs <- word
    } yield x +: xs

    nonEmptyWord <|> Monad[Parser].unit("")
  }
  def string(s: String): Parser[String] = {
    if (s.isEmpty) Monad[Parser].unit("")
    else
      for {
        x <- char(s.head)
        xs <- string(s.tail)
      } yield x +: xs
  }

  def many[A](p: Parser[A]): Parser[List[A]] =
    (for {
      x <- p
      xs <- many(p)
    } yield x +: xs) <|> Monad[Parser].unit(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      x <- p
      xs <- many(p)
    } yield x +: xs

  def sepBy1[A, B](pa: Parser[A], sep: Parser[B]): Parser[List[A]] =
    for {
      x <- pa
      xs <- many(for {
        _ <- sep
        y <- pa
      } yield y)
    } yield x :: xs
}
