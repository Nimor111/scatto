package scatto

import org.scalatest._
import scatto.parser.Lexer

class LexerTest extends FlatSpec with Matchers {
  "space" should "parse whitespaces" in {
    Lexer.space("  2").head shouldEqual ("  ", "2")
  }

  "ident" should "parse valid identifiers" in {
    Lexer.ident("_a").head shouldEqual ("_a", "")
    Lexer.ident("a").head shouldEqual ("a", "")
    Lexer.ident("scAt_to").head shouldEqual ("scAt_to", "")
    Lexer.ident("ClassName").head shouldEqual ("ClassName", "")
  }

  it should "not parse invalid identifiers" in {
    Lexer.ident("1a") shouldEqual List()
    Lexer.ident("$a") shouldEqual List()
  }

  "nat" should "parse natural numbers" in {
    Lexer.nat("123").head shouldEqual (123, "")
  }

  it should "fail on negative numbers" in {
    Lexer.nat("-123") shouldEqual List()
  }

  "int" should "parse positive and negative numbers" in {
    Lexer.int("123").head shouldEqual (123, "")
    Lexer.int("-123").head shouldEqual (-123, "")
  }

  "double" should "parse floating point numbers" in {
    Lexer.double("123.456").head shouldEqual (123.456, "")
  }

  it should "fail on integers" in {
    Lexer.double("123") shouldEqual List()
  }

  "char" should "parse a character" in {
    Lexer.char('c')("cd").head shouldEqual ('c', "d")
  }

  it should "fail on wrong character" in {
    Lexer.char('c')("dc") shouldEqual List()
  }

  "string" should "parse a string" in {
    Lexer.string("string")("stringer").head shouldEqual ("string", "er")
    Lexer.string("")("string").head shouldEqual ("", "string")
  }

  "array" should "parse a non-empty array of ints"  in {
    Lexer.array(Lexer.int)("[1,2,3]").head shouldEqual (List(1, 2, 3), "")
  }

  "bracket" should "parse a string in brackets" in {
    Lexer.bracket(Lexer.char('('), Lexer.string("string"), Lexer.char(')'))("(string)").head shouldEqual ("string", "")
  }
}
