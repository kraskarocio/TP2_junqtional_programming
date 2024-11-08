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
            case l: List[_] => "[" + l.map {
              case elem: Number => elem.toString
              case elem => "\"" + elem.toString.replace("\"", "\\\"") + "\""
            }.mkString(", ") + "]"
            case null => "null"
            case m: Map[_, _] => mapToJsonString(m.asInstanceOf[Map[String, Any]])  // Recursión para Map
            case _ => "\"" + value.toString.replace("\"", "\\\"") + "\""
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
        case m: Map[_, _] => mapToJsonString(m.asInstanceOf[Map[String, Any]])  // Recursión para Map
        case l: List[_] => mapToJsonString(l)
        case _ => "\"" + l.toString.replace("\"", "\\\"") + "\""
      }
      "[" + jsonParts.mkString(", ") + "]"

    case _ =>
      "\"" + map.toString.replace("\"", "\\\"") + "\""
  }
}

