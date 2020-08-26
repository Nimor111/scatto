package scatto

import org.scalatest._
import flatspec._
import matchers._
import scatto.parser.Lexer

class LexerTest extends AnyFlatSpec with should.Matchers {
  "space" should "parse whitespaces" in {
    Lexer.space("  2") shouldEqual List(("  ", "2"))
  }

  "spaceBefore" should "ignore whitespace in front" in {
    Lexer.spaceBefore(Lexer.char('a'))("  a")
  }

  "spaceAfter" should "ignore whitespace after" in {
    Lexer.spaceAfter(Lexer.char('a'))("a  ")
  }

  "trim" should "ignore whitespace before and after" in {
    Lexer.trim(Lexer.char('a'))("  a  ")
  }

  "ident" should "parse valid identifiers" in {
    Lexer.ident("_a") shouldEqual List(("_a", ""))
    Lexer.ident("a") shouldEqual List(("a", ""))
    Lexer.ident("scAt_to") shouldEqual List(("scAt_to", ""))
    Lexer.ident("ClassName") shouldEqual List(("ClassName", ""))
  }

  it should "not parse invalid identifiers" in {
    Lexer.ident("1a") shouldEqual List()
    Lexer.ident("$a") shouldEqual List()
  }

  "keyword" should "parse a keyword" in {
    Lexer.keyword(List("var", "val"))("var") shouldEqual List(("var", ""))
  }

  it should "ignore non-keywords" in {
    Lexer.keyword(List("var", "val"))("name") shouldEqual List()
  }

  "nat" should "parse natural numbers" in {
    Lexer.nat("123") shouldEqual List((123, ""))
  }

  it should "fail on negative numbers" in {
    Lexer.nat("-123") shouldEqual List()
  }

  "int" should "parse positive and negative numbers" in {
    Lexer.int("123") shouldEqual List((123, ""))
    Lexer.int("-123") shouldEqual List((-123, ""))
  }

  "double" should "parse floating point numbers" in {
    Lexer.double("123.456") shouldEqual List((123.456, ""))
  }

  it should "fail on integers" in {
    Lexer.double("123") shouldEqual List()
  }

  "char" should "parse a character" in {
    Lexer.char('c')("cd") shouldEqual List(('c', "d"))
  }

  it should "fail on wrong character" in {
    Lexer.char('c')("dc") shouldEqual List()
  }

  "string" should "parse a string" in {
    Lexer.string("string")("stringer") shouldEqual List(("string", "er"))
    Lexer.string("")("string") shouldEqual List(("", "string"))
  }

  "array" should "parse a non-empty array of ints" in {
    Lexer.array(Lexer.int)("[1,2,3]") shouldEqual List((List(1, 2, 3), ""))
  }

  "bracket" should "parse a string in brackets" in {
    Lexer
      .bracket(Lexer.char('('), Lexer.string("string"), Lexer.char(')'))(
        "(string)"
      ) shouldEqual List(("string", ""))
  }
}
