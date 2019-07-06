package scatto.examples

import scala.math.BigDecimal

import scatto.parser.types._
import scatto.parser.Combinators
import scatto.parser.Lexer
import scatto.parser.MonadInstances._
import scatto.parser.MonadPlusInstances._

sealed trait JsonValue

case object JsonNull extends JsonValue
case class JsonInt(value: BigDecimal) extends JsonValue
case class JsonDouble(value: Double) extends JsonValue
case class JsonString(string: String) extends JsonValue
case class JsonBoolean(bool: Boolean) extends JsonValue
case class JsonArray(value: List[JsonValue]) extends JsonValue
case class JsonObject(value: Map[String, JsonValue]) extends JsonValue

object JsonParser {
  def jsonNull: Parser[JsonValue] =
    for {
      _ <- Lexer.string("null")
    } yield JsonNull

  def jsonNumber: Parser[JsonValue] =
    for {
      d <- Combinators.many1(Combinators.digit)
    } yield JsonInt(BigDecimal(d.mkString))

  def jsonDouble: Parser[JsonValue] =
    for {
      d <- Lexer.double
    } yield JsonDouble(d)

  def jsonString: Parser[JsonValue] =
    for {
      _ <- Lexer.string("\"")
      s <- Lexer.word
      _ <- Lexer.string("\"")
    } yield JsonString(s)

  def jsonBoolean: Parser[JsonValue] =
    for {
      b <- Lexer.string("true") +++ Lexer.string("false")
    } yield JsonBoolean(b.toBoolean)

  def jsonValue: Parser[JsonValue] =
    jsonNull +++ jsonBoolean +++ jsonString +++ jsonDouble +++ jsonNumber +++ jsonObject +++ jsonArray

  def jsonArray: Parser[JsonValue] =
    for {
      _ <- Lexer.char('[')
      arr <- Combinators.sepBy(jsonValue, Lexer.trim(Lexer.char(',')))
      _ <- Lexer.char(']')
    } yield JsonArray(arr)

  def jsonObject: Parser[JsonValue] = {
    def obj: Parser[(String, JsonValue)] =
      for {
        _ <- Lexer.string("\"")
        key <- Lexer.word
        _ <- Lexer.string("\"")
        _ <- Lexer.spaceBefore(Lexer.char(':'))
        v <- Lexer.spaceBefore(jsonValue)
      } yield (key, v)

    for {
      arr <- Lexer.bracket(
        Lexer.char('{'),
        Combinators
          .sepBy(obj, Lexer.trim(Lexer.char(','))),
        Lexer.char('}')
      )
    } yield JsonObject(arr.toMap)
  }
}

object JsonApp extends App {
  println(
    parseEither(
      JsonParser.jsonObject,
      "{\"key\": [1, 2, 3], \"key1\": 1, \"key2.5\": 2.5, \"key2\": {\"key3\": \"value1\"}, \"key3\": true}"
    )
  )
}
