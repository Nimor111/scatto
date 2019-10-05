package scatto.examples.applicative

import scatto.parser.types._
import scatto.parser.Combinators
import scatto.parser.Lexer
import scatto.parser.MonadInstances._
import scatto.parser.MonadPlusInstances._
import scatto.parser.AlternativeInstances._
import scatto.parser.ApplicativeInstances._
import scatto.parser.Applicative

import scatto.examples.types.LogTypes._

object LogParser {
  val ap = Applicative[Parser]

  def dateParser: Parser[Date] = {
    val year = Combinators.count(4, Combinators.digit)
    val monthDay = Combinators.count(2, Combinators.digit)
    val hyphen = Lexer.char('-')

    def date(year: List[Char])(month: List[Char])(day: List[Char]) =
      Date(day.mkString.toInt, month.mkString.toInt, year.mkString.toInt)

    ap.pure(date _) <*> year <* hyphen <*> monthDay <* hyphen <*> monthDay
  }

  def timeParser: Parser[Time] = {
    val timeP = Combinators.count(2, Combinators.digit)
    val colon = Lexer.char(':')

    def time(hours: List[Char])(minutes: List[Char])(seconds: List[Char]) =
      Time(seconds.mkString.toInt, minutes.mkString.toInt, hours.mkString.toInt)

    ap.pure(time _) <*> timeP <* colon <*> timeP <* colon <*> timeP
  }

  def ipParser: Parser[IP] = {
    val num = Lexer.nat
    val dot = Lexer.char('.')

    def ip(first: Int)(second: Int)(third: Int)(fourth: Int) =
      IP(first, second, third, fourth)

    ap.pure(ip _) <*> num <* dot <*> num <* dot <*> num <* dot <*> num
  }

  def productParser: Parser[Product] = {
    def computerParser: Parser[Product] =
      Lexer.string("computer") *> ap.pure(Computer)

    def keyboardParser: Parser[Product] =
      Lexer.string("keyboard") *> ap.pure(Keyboard)

    def mouseParser: Parser[Product] =
      Lexer.string("mouse") *> ap.pure(Mouse)

    def laptopParser: Parser[Product] =
      Lexer.string("laptop") *> ap.pure(Laptop)

    computerParser <|> keyboardParser <|> mouseParser <|> laptopParser
  }

  def logParser: Parser[Log] = {
    def log(date: Date)(time: Time)(ip: IP)(product: Product) =
      Log(date, time, ip, product)
    val space = Lexer.space

    ap.pure(log _) <*> dateParser <* space <*> timeParser <* space <*> ipParser <* space <*> productParser
  }
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
