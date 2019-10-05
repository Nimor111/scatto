package scatto.parser

import scatto.parser.types.Parser

import scala.language.higherKinds

trait Alternative[F[_]] extends Applicative[F] {
  def empty[A]: F[A]
  def aplus[A](fa: F[A], fb: F[A]): F[A]
  def withFilter[A](fa: F[A])(f: A => Boolean): F[A]
}

object Alternative {
  def apply[F[_]](implicit alt: Alternative[F]): Alternative[F] = alt
}

object AlternativeInstances {
  implicit class AlternativeOps[F[_]: Alternative, A](fa: F[A]) {
    def <|>(fb: F[A]): F[A] = {
      implicitly[Alternative[F]].aplus(fa, fb)
    }
    def withFilter(f: A => Boolean): F[A] =
      implicitly[Alternative[F]].withFilter(fa)(f)
  }

  implicit def parserAlternative(
      implicit
      applicative: Applicative[Parser],
      mp: MonadPlus[Parser]
  ): Alternative[Parser] = new Alternative[Parser] {
    def pure[A](a: A): Parser[A] = applicative.pure(a)
    def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
      applicative.ap(ff)(fa)

    def empty[A]: Parser[A] = mp.mzero
    def aplus[A](pa: Parser[A], pb: Parser[A]): Parser[A] =
      Combinators.first(mp.mplus(pa, pb))
    def withFilter[A](pa: Parser[A])(f: A => Boolean): Parser[A] =
      mp.withFilter(pa)(f)
  }
}
