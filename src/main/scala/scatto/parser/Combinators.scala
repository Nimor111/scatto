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
  def first[A](p: Parser[A]): Parser[A] =
    input =>
      p(input) match {
        case List() => List()
        case x :: _ => List(x)
      }

  def digit: Parser[Char] = satisfy(x => x >= '0' && x <= '9')
  def lower: Parser[Char] = satisfy(x => x >= 'a' && x <= 'z')
  def upper: Parser[Char] = satisfy(x => x >= 'A' && x <= 'Z')

  def letter: Parser[Char] = lower +++ upper
  def alphanum: Parser[Char] = letter +++ digit
  def word: Parser[String] = {
    val nonEmptyWord = for {
      x <- letter
      xs <- word
    } yield x +: xs

    nonEmptyWord +++ Monad[Parser].unit("")
  }

  def many[A](p: Parser[A]): Parser[List[A]] =
    (for {
      x <- p
      xs <- many(p)
    } yield x +: xs) +++ Monad[Parser].unit(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      x <- p
      xs <- many(p)
    } yield x +: xs

  def sepBy[A, B](pa: Parser[A], sep: Parser[B]): Parser[List[A]] =
    sepBy1(pa, sep) +++ Monad[Parser].unit(List())

  def sepBy1[A, B](pa: Parser[A], sep: Parser[B]): Parser[List[A]] =
    for {
      x <- pa
      xs <- many(for {
        _ <- sep
        y <- pa
      } yield y)
    } yield x :: xs

  def chainl1[A](p: Parser[A], op: Parser[(A, A) => A]): Parser[A] = {
    def rest(x: A): Parser[A] =
      op.flatMap(operation => p.flatMap(y => rest(operation(x, y)))) +++ Monad[
        Parser
      ].unit(x)

    p.flatMap(rest)
  }
}
