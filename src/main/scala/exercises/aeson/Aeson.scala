package exercises.aeson

sealed trait JSONValue:
  def stringify: String

final case class JSONString(value: String) extends JSONValue:
  def stringify: String = "\"" + value.replace("\"", "\\\"") + "\""

final case class JSONNumber(value: Int) extends JSONValue:
  def stringify: String = value.toString

final case class JSONBool(value: Boolean) extends JSONValue:
  def stringify: String = if value then "true" else "false"

final case class JSONArray(values: List[JSONValue]) extends JSONValue:
  def stringify: String = values.map(_.stringify).mkString("[", ", ", "]")

final case class JSONObject(value: Map[String, JSONValue]) extends JSONValue:
  def stringify: String =
    def elemToJSONEntry (elem: (String, JSONValue)) =
      "\"" + elem._1 + "\": " + elem._2.stringify
    value.map(elemToJSONEntry).mkString("{", ", ", "}")

trait ToJSONValue[T]:
  def toJSONValue(t: T): JSONValue

implicit object ToJSONNumber extends ToJSONValue[Int]:
  def toJSONValue(i: Int): JSONValue = JSONNumber(i)

implicit object ToJSONString extends ToJSONValue[String]:
  def toJSONValue(s: String): JSONValue = JSONString(s)

implicit object ToJSONBool extends ToJSONValue[Boolean]:
  def toJSONValue(b: Boolean): JSONValue = JSONBool(b)

implicit class Aeson[T](obj: T):
  def toJSON(implicit converter: ToJSONValue[T]): JSONValue =
    converter.toJSONValue(obj)

object AesonTest extends App:
  val b = JSONBool(true)
  val i = JSONNumber(42)
  val s = JSONString("ss")
  val a = JSONArray(List(b, i))
  val o = JSONObject(Map("arr" -> a, "texto" -> s))
  println(o.stringify)
