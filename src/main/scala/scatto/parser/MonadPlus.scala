package scatto.parser

import scatto.parser.types.Parser

import scala.language.higherKinds

trait MonadPlus[F[_]] extends Monad[F] {
  def mzero[A]: F[A]
  def mplus[A](fa: F[A], fb: F[A]): F[A]
  def withFilter[A](fa: F[A])(f: A => Boolean): F[A]
}

object MonadPlus {
  def apply[F[_]](implicit mp: MonadPlus[F]): MonadPlus[F] = mp
}

object MonadPlusInstances {
  implicit class MonadPlusOps[F[_]: MonadPlus, A](fa: F[A]) {
    def <|>(fb: F[A]): F[A] = {
      MonadPlus[F].mplus(fa, fb)
    }
    def mzero[B]: F[B] = MonadPlus[F].mzero
    def withFilter(f: A => Boolean): F[A] = MonadPlus[F].withFilter(fa)(f)
  }

  implicit def parserMonadPlus(implicit m: Monad[Parser]): MonadPlus[Parser] =
    new MonadPlus[Parser] {
      def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] =
        m.flatMap(pa)(f)
      def unit[A](a: A): Parser[A] = m.unit(a)
      def map[A, B](pa: Parser[A])(f: A => B): Parser[B] = m.map(pa)(f)

      def mzero[A]: Parser[A] = _ => List()
      def mplus[A](pa: Parser[A], pb: Parser[A]): Parser[A] =
        input => pa(input) ++ pb(input)
      def withFilter[A](pa: Parser[A])(f: A => Boolean): Parser[A] =
        flatMap(pa)(a => if (f(a)) unit(a) else mzero)
    }
}
