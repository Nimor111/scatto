package scatto.parser

package object types {
  type Parser[A] = String => List[(A, String)]
}
