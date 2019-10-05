package scatto.examples.monadic

import scatto.parser.types._
import scatto.parser.Combinators
import scatto.parser.Lexer
import scatto.parser.MonadInstances._
import scatto.parser.MonadPlusInstances._

import scatto.examples.types.LogTypes._

object LogParser {
  def dateParser: Parser[Date] =
    for {
      year <- Combinators.count(4, Combinators.digit)
      _ <- Lexer.char('-')
      month <- Combinators.count(2, Combinators.digit)
      _ <- Lexer.char('-')
      day <- Combinators.count(2, Combinators.digit)
    } yield Date(day.mkString.toInt, month.mkString.toInt, year.mkString.toInt)

  def timeParser: Parser[Time] =
    for {
      hours <- Combinators.count(2, Combinators.digit)
      _ <- Lexer.char(':')
      minutes <- Combinators.count(2, Combinators.digit)
      _ <- Lexer.char(':')
      seconds <- Combinators.count(2, Combinators.digit)
    } yield
      Time(seconds.mkString.toInt, minutes.mkString.toInt, hours.mkString.toInt)

  def ipParser: Parser[IP] =
    for {
      first <- Lexer.nat
      _ <- Lexer.char('.')
      second <- Lexer.nat
      _ <- Lexer.char('.')
      third <- Lexer.nat
      _ <- Lexer.char('.')
      fourth <- Lexer.nat
    } yield IP(first, second, third, fourth)

  def productParser: Parser[Product] = {
    def computerParser: Parser[Product] =
      for {
        _ <- Lexer.string("computer")
      } yield Computer

    def keyboardParser: Parser[Product] =
      for {
        _ <- Lexer.string("keyboard")
      } yield Keyboard

    def mouseParser: Parser[Product] =
      for {
        _ <- Lexer.string("mouse")
      } yield Mouse

    def laptopParser: Parser[Product] =
      for {
        _ <- Lexer.string("laptop")
      } yield Laptop

    computerParser +++ keyboardParser +++ mouseParser +++ laptopParser
  }

  def logParser: Parser[Log] =
    for {
      date <- dateParser
      _ <- Lexer.space
      time <- timeParser
      _ <- Lexer.space
      ip <- ipParser
      _ <- Lexer.space
      product <- productParser
    } yield Log(date, time, ip, product)
}

object LogApp extends App {
  val logs = List(
    "2013-06-29 11:16:23 124.67.34.60 keyboard",
    "2013-06-29 11:32:12 212.141.23.67 mouse",
    "2013-06-29 11:33:08 212.141.23.67 laptop",
    "2013-06-29 12:12:34 125.80.32.31 computer",
    "2013-06-29 12:51:50 101.40.50.62 keyboard",
    "2013-06-29 13:10:45 103.29.60.13 mouse"
  )

  logs.foreach(l => println(parseEither(LogParser.logParser, l)))
}
