package scatto.parser

package object types {
  type Parser[A] = String => List[(A, String)]

  def parseEither[A](p: Parser[A], input: String): Either[String, A] =
    p(input) match {
      case List() => Left("Error while parsing.")
      case (v, _) :: _ => Right(v)
    }

  def parse[A](p: Parser[A], input: String): Either[String, (A, String)] =
    p(input) match {
      case List() => Left("Error while parsing")
      case x :: _ => Right(x)
    }
}
