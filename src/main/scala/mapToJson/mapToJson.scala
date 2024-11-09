package mapToJson

def mapToJsonString(map: Any): String = {
  map match {
    case m: Map[String, Any] =>
      val jsonParts = m.map {
        case (key, value) =>
          val jsonValue = value match {
            case s: String => "\"" + s.replace("\"", "\\\"") + "\""
            case n: Number => n.toString
            case b: Boolean => b.toString
            case l: List[_] => "[" + l.map(mapToJsonString).mkString(", ") + "]"
            case null => "null"
            case nestedMap: Map[_, _] => mapToJsonString(nestedMap.asInstanceOf[Map[String, Any]])
            case other => "\"" + other.toString.replace("\"", "\\\"") + "\""
          }
          "\"" + key.replace("\"", "\\\"") + "\": " + jsonValue
      }
      "{" + jsonParts.mkString(", ") + "}"

    case l: List[Any] =>
      val jsonParts = l.map {
        case s: String => "\"" + s.replace("\"", "\\\"") + "\""
        case n: Number => n.toString
        case b: Boolean => b.toString
        case null => "null"
        case m: Map[_, _] => mapToJsonString(m.asInstanceOf[Map[String, Any]])  // Recursion for Map
        case nestedList: List[_] => mapToJsonString(nestedList)
        case other => "\"" + other.toString.replace("\"", "\\\"") + "\""
      }
      "[" + jsonParts.mkString(", ") + "]"

    case s: String => "\"" + s.replace("\"", "\\\"") + "\""
    case n: Number => n.toString
    case b: Boolean => b.toString
    case null => "null"
    case other => "\"" + other.toString.replace("\"", "\\\"") + "\""
  }
}

