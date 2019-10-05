package scatto.examples.types

import scala.math.BigDecimal

object JsonTypes {
  sealed trait JsonValue

  case object JsonNull extends JsonValue
  case class JsonInt(value: BigDecimal) extends JsonValue
  case class JsonDouble(value: Double) extends JsonValue
  case class JsonString(string: String) extends JsonValue
  case class JsonBoolean(bool: Boolean) extends JsonValue
  case class JsonArray(value: List[JsonValue]) extends JsonValue
  case class JsonObject(value: Map[String, JsonValue]) extends JsonValue
}
