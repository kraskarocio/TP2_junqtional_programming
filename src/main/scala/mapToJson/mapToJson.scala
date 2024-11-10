package mapToJson
def mapToJsonString(map: Any): String = {
  map match {
    case m: Map[String, Any] =>
      val jsonParts = m.map {
        case (key, value) =>
          val jsonValue = mapToJsonString(value)
          "'" + key + "' : " + jsonValue
      }
      "{ " + jsonParts.mkString(", ") + " } "

    case l: List[Any] =>
      val jsonParts = l.map(mapToJsonString)
      "[" + jsonParts.mkString(", ") + "]"

    case s: String =>  s.replace("\"", "\\\"")
    case n: Number => n.toString
    case b: Boolean => b.toString
    case null => "null"
    case other => other.toString.replace("\"", "\\\"")
  }
}
