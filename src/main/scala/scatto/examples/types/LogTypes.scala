package scatto.examples.types

/*
  Logs are in the form:
  yyyy-mm-dd hh:mm:ss 0.0.0.0 product
 */

object LogTypes {
  sealed trait Product

  case object Computer extends Product
  case object Keyboard extends Product
  case object Mouse extends Product
  case object Laptop extends Product

  case class Date(day: Int, month: Int, year: Int)
  case class Time(seconds: Int, minutes: Int, hours: Int)
  case class IP(first: Int, second: Int, third: Int, fourth: Int)

  case class Log(date: Date, time: Time, ip: IP, product: Product)
}
