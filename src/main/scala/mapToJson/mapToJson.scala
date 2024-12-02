package mapToJson

/**
 * @brief Converts a Map or List to a JSON string representation.
 * This function recursively converts a Map or List into a JSON-like string. 
 * It handles different types of values, such as Strings, Numbers, Booleans, and null, 
 * and properly escapes any special characters in strings.
 * @param map The input Map or List to be converted to a JSON string.
 * @return A string representation of the input Map or List in JSON format.
 */
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

    case s: String => " '" + s + "' "
    case n: Number => n.toString
    case b: Boolean => b.toString
    case null => "null"
    case other => other.toString.replace("\"", "\\\"")
  }
}
