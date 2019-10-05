package scatto.parser

import scatto.parser.types.Parser
import scatto.parser.MonadInstances._
import scatto.parser.Monad._

trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
}

object Applicative {
  def apply[F[_]](implicit a: Applicative[F]): Applicative[F] = a
}

object ApplicativeInstances {
  implicit class ApplicativeOps[F[_]: Applicative, A, B](ff: F[A => B]) {
    def <*>(fa: F[A]): F[B] = {
      implicitly[Applicative[F]].ap(ff)(fa)
    }
  }

  implicit class ApplicativeDiscardOps[F[_]: Applicative, A, B](fa: F[A]) {
    def <*(fb: F[B]): F[A] = {
      def const[A, B](a: A)(b: B): A = a

      implicitly[Applicative[F]].pure(const: A => (B => A)) <*> fa <*> fb
    }

    def *>(fb: F[B]): F[B] = {
      def flipConst[A, B](a: A)(b: B) = b

      implicitly[Applicative[F]].pure(flipConst: A => (B => B)) <*> fa <*> fb
    }
  }

  implicit val parserApplicative: Applicative[Parser] =
    new Applicative[Parser] {
      def pure[A](v: A): Parser[A] = input => List((v, input))
      def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = input => {
        val acc = List[(B, String)]()
        ff(input).foldRight(acc)((res: (A => B, String), a) => {
          val (f, out) = res
          val res2 = fa(out)
          val applied = res2.map((r => (f(r._1), r._2)))
          applied ++ a
        })
      }
    }
}
