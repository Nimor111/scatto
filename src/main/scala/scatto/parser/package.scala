package scatto.parser

package object types {
  type Parser[A] = String => List[(A, String)]

  def parseEither[A](p: Parser[A], input: String): Either[String, A] =
    p(input) match {
      case List() => Left("Error while parsing.")
      case List((v, _)) => Right(v)
    }
}
