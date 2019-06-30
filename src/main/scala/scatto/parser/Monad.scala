package scatto.parser

import scatto.parser.types.Parser

import scala.language.higherKinds

trait Monad[F[_]] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
  def map[A, B](m: F[A])(f: A => B): F[B]
}

object Monad {
  def apply[F[_]](implicit m: Monad[F]): Monad[F] = m
}

object MonadInstances {
  implicit class MonadOps[F[_]: Monad, A](fa: F[A]) {
    def flatMap[B](f: A => F[B])(implicit m: Monad[F]): F[B] = {
      m.flatMap(fa)(f)
    }
    def map[B](f: A => B)(implicit m: Monad[F]): F[B] = {
      m.map(fa)(f)
    }
    def unit[B](a: B)(implicit m: Monad[F]): F[B] = {
      m.unit(a)
    }
  }

  implicit val parserMonad: Monad[Parser] = new Monad[Parser] {
    def unit[A](v: A): Parser[A] = input => List((v, input))
    def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] = input => {
      val acc = List(List[(B, String)]())
      pa(input)
        .foldRight(acc)((res: (A, String), a) => f(res._1)(res._2) :: a)
        .flatten
    }

    def map[A, B](pa: Parser[A])(f: A => B): Parser[B] = input => {
      pa(input).foldRight(List[(B, String)]())((res, acc) =>
        (f(res._1), res._2) :: acc)
    }
  }
}
