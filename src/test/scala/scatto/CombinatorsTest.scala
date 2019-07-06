package scatto

import org.scalatest._
import scatto.parser.{Combinators, Lexer, Monad}
import scatto.parser.types._
import scatto.parser.MonadInstances._

class CombinatorsTest extends FlatSpec with Matchers {
  "satisfy" should "parse correct characters" in {
    Combinators.satisfy(x => x == 'a')("a") shouldEqual List(('a', ""))
  }

  it should "fail on incorrect characters" in {
    Combinators.satisfy(x => x == 'a')("b") shouldEqual List()
  }

  "digit" should "parse a digit correctly" in {
    Combinators.digit("2") shouldEqual List(('2', ""))
  }

  "lower" should "parse a lowercase letter" in {
    Combinators.lower("a") shouldEqual List(('a', ""))
  }

  it should "not parse an uppercase letter" in {
    Combinators.lower("A") shouldEqual List()
  }

  "upper" should "parse an uppercase digit" in {
    Combinators.upper("A") shouldEqual List(('A', ""))
  }

  it should "not parse a lowercase digit" in {
    Combinators.upper("a") shouldEqual List()
  }

  "letter" should "parse upper and lowercase letters" in {
    Combinators.letter("a") shouldEqual List(('a', ""))
    Combinators.letter("A") shouldEqual List(('A', ""))
  }

  "alphanum" should "parse a letter or a digit" in {
    Combinators.alphanum("1") shouldEqual List(('1', ""))
    Combinators.alphanum("a") shouldEqual List(('a', ""))
  }

  "many" should "parse a series of characters" in {
    Combinators
      .many(Combinators.digit)("123")
      .head shouldEqual (List('1', '2', '3'), "")
    Combinators
      .many(Combinators.letter)("word")
      .head shouldEqual (List('w', 'o', 'r', 'd'), "")
    Combinators
      .many(Combinators.alphanum)("1w2o3r4d")
      .head shouldEqual (List('1', 'w', '2', 'o', '3', 'r', '4', 'd'), "")
  }

  it should "parse an empty sequence" in {
    Combinators.many(Combinators.letter)("").head shouldEqual (List(), "")
  }

  "many1" should "not parse an empty sequence" in {
    Combinators.many1(Combinators.letter)("") shouldEqual List()
  }

  "sepBy" should "parse a comma separated string of digits" in {
    Combinators
      .sepBy(Combinators.digit, Lexer.char(','))("1,2,3")
      .head shouldEqual (List('1', '2', '3'), "")
  }

  "chainl1" should "parse an expression with plus" in {
    def add: Parser[(Int, Int) => Int] =
      for {
        _ <- Combinators.satisfy(x => x == '+')
      } yield (x: Int, y: Int) => x + y

    Combinators
      .chainl1(Lexer.nat, add)("1+2+3")
      .head shouldEqual (6, "")
  }

  "count" should "take a fixed amount of characters" in {
    Combinators.count(2, Combinators.digit)("123") shouldEqual List(
      (List('1', '2'), "3")
    )
    Combinators.count(0, Combinators.digit)("123") shouldEqual List(
      (List(), "123")
    )
  }
}
