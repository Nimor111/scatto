package scatto.parser

import scatto.parser.types.Parser
import scatto.parser.MonadInstances._

class Combinators {
  def result[A](v: A): Parser[A] = input => List((v, input))
  def zero[A]: Parser[A] = _ => List()
  def item: Parser[Char] =
    input =>
      input.headOption match {
        case None => List()
        case Some(x) => List((x, input.tail))
      }
  def bind[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] = input => {
    val acc = List(List[(B, String)]())
    pa(input)
      .foldRight(acc)((res: (A, String), a) => f(res._1)(res._2) :: a)
      .flatten
  }
  def satisfy(pred: Char => Boolean): Parser[Char] =
    bind(item)((x: Char) => if (pred(x)) result(x) else zero)

  def char(c: Char): Parser[Char] = satisfy(y => c == y)
  def digit: Parser[Char] = satisfy(x => x >= '0' && x <= '9')
  def lower: Parser[Char] = satisfy(x => x >= 'a' && x <= 'z')
  def upper: Parser[Char] = satisfy(x => x >= 'A' && x <= 'Z')

  // choice combinator
  def plus[A](pa: Parser[A], pb: Parser[A]): Parser[A] =
    input => pa(input) ++ pb(input)

  def letter: Parser[Char] = plus(lower, upper)
  def alphanum: Parser[Char] = plus(letter, digit)
  def word: Parser[String] = {
    val nonEmptyWord = for {
      x <- letter
      xs <- word
    } yield x +: xs

    plus(nonEmptyWord, result(""))
  }
  def string(s: String): Parser[String] = {
    s.headOption match {
      case None => result("")
      case Some(h) =>
        for {
          x <- char(h)
          xs <- string(s.tail)
        } yield x +: xs
    }
  }

  def many[A](p: Parser[A]): Parser[List[A]] =
    plus(for {
      x <- p
      xs <- many(p)
    } yield x +: xs, result(List()))
  def many1[A](p: Parser[A]): Parser[List[A]] =
    for {
      x <- p
      xs <- many(p)
    } yield x +: xs

}

object Combinators {}
