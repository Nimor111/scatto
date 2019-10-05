package scatto.examples.applicative

import scatto.parser.ApplicativeInstances._
import scatto.parser.AlternativeInstances._
import scatto.parser.MonadInstances._
import scatto.parser.MonadPlusInstances._
import scatto.parser.Lexer
import scatto.parser.Combinators
import scatto.parser.types._
import scatto.parser.Applicative
import scatto.parser.Alternative

import scatto.examples.types.JsonTypes._

object JsonParser {
  val ap = Applicative[Parser]

  def jsonNull: Parser[JsonValue] =
    Lexer.string("null") *> ap.pure(JsonNull)

  def jsonNumber: Parser[JsonValue] = {
    val f = ((a: List[Char]) => a.mkString)
      .andThen((s: String) => BigDecimal(s))
      .andThen((n: BigDecimal) => JsonInt(n))

    ap.pure(f) <*> Combinators.many1(Combinators.digit)
  }

  def jsonDouble: Parser[JsonValue] =
    ap.pure((d: Double) => JsonDouble(d)) <*> Lexer.double

  def jsonString: Parser[JsonString] =
    Lexer.string("\"") *> ap.pure((s: String) => JsonString(s)) <*> Lexer.word <* Lexer
      .string("\"")

  def jsonBoolean: Parser[JsonValue] =
    ap.pure((b: String) => JsonBoolean(b.toBoolean)) <*>
      (Lexer.string("true") <|> Lexer.string("false"))

  def jsonValue: Parser[JsonValue] =
    jsonNull <|> jsonBoolean <|> jsonString <|> jsonDouble <|> jsonNumber <|> jsonObject <|> jsonArray

  def jsonArray: Parser[JsonValue] =
    Lexer.char('[') *>
      ap.pure((arr: List[JsonValue]) => JsonArray(arr)) <*>
      Combinators.sepBy(jsonValue, Lexer.trim(Lexer.char(','))) <*
      Lexer.char(']')

  def jsonObject: Parser[JsonValue] = {
    def key: Parser[String] =
      Lexer.string("\"") *>
        ap.pure((k: String) => k) <*>
        Lexer.word <*
        Lexer.string("\"") <*
        Lexer.spaceBefore(Lexer.char(':'))

    def value: Parser[JsonValue] =
      Lexer.spaceBefore(jsonValue)

    def obj: Parser[(String, JsonValue)] = {
      def f(k: String)(v: JsonValue) = (k, v)

      ap.pure(f _) <*>
        key <*>
        value
    }

    Lexer.char('{') *>
      ap.pure((arr: List[(String, JsonValue)]) => JsonObject(arr.toMap)) <*>
      Combinators.sepBy(obj, Lexer.trim(Lexer.char(','))) <*
      Lexer.char('}')
  }
}

object JsonApp extends App {
  println(
    parseEither(
      //JsonParser.jsonObject,
      //"{\"key\": [1, 2, 3], \"key1\": 1, \"key2.5\": 2.5, \"key2\": {\"key3\": \"value1\"}, \"key3\": true, \"key4\": null}"))
      JsonParser.jsonObject,
      "{\"key\": 1}"
    )
  )
}
