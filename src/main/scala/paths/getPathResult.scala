package paths
import paths.{tokenize, extractTokens, PathToken}
import paths.PathToken.*
import mapToJson.mapToJsonString


def navigateRecursive(tokens: List[(PathToken, String)], currentJson: Any): Any = tokens match {

  case List((PathToken.DOT, _)) => mapToJsonString(currentJson.asInstanceOf[Map[String, Any]])
  case (PathToken.DOT, _) :: (PathToken.STR, key1) :: (PathToken.DOT, _) :: (PathToken.STR, key2) :: nil =>
    currentJson match {
      case obj: Map[String, Any] =>
        obj.get(key1) match {
          case Some(innerObj: Map[String, Any]) =>
            innerObj.get(key2) match {
              case Some(value) => removeKeyAndBrackets(mapToJsonString(Map(key2 -> value)))
              case None => throw new NoSuchElementException(s"Key '$key2' not found in JSON object under '$key1'")
            }
          case Some(_) => throw new IllegalArgumentException(s"The key '$key1' does not contain a JSON object")
          case None => throw new NoSuchElementException(s"Key '$key1' not found in JSON object")
        }
      case _ => throw new IllegalArgumentException("Expected a JSON object at this path")
    }
  case (PathToken.DOT, _) :: (PathToken.STR, key1) :: (PathToken.PIPELINE, _) :: (PathToken.DOT, _) :: (PathToken.STR, key2) :: nil =>
    currentJson match {
      case obj: Map[String, Any] =>
        obj.get(key1) match {
          case Some(innerObj: Map[String, Any]) =>
            innerObj.get(key2) match {
              case Some(value) => removeKeyAndBrackets(mapToJsonString(Map(key2 -> value)))
              case None => throw new NoSuchElementException(s"Key '$key2' not found in JSON object under '$key1'")
            }
          case Some(_) => throw new IllegalArgumentException(s"The key '$key1' does not contain a JSON object")
          case None => throw new NoSuchElementException(s"Key '$key1' not found in JSON object")
        }
      case _ => throw new IllegalArgumentException("Expected a JSON object at this path")
    }
  case (PathToken.DOT, _) :: (PathToken.STR, key) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: Nil =>
    currentJson match {
      case obj: Map[String, Any] =>
        obj.get(key) match {
          case Some(innerList: List[Any]) =>
            if (pos.toInt >= 0 && pos.toInt < innerList.size) {
              // Acceso seguro al índice dentro de la lista
              val value = innerList(pos.toInt)
              removeKeyAndBrackets(mapToJsonString(Map(pos -> value)))
            } else {
              throw new IndexOutOfBoundsException(s"Index '$pos' is out of bounds for the list under key '$key'")
            }

          case Some(_) => throw new IllegalArgumentException(s"The key '$key' does not contain a JSON array")
          case None => throw new NoSuchElementException(s"Key '$key' not found in JSON object")
        }
      case _ => throw new IllegalArgumentException("Expected a JSON object at this path")
    }
  case (PathToken.DOT, _) :: (PathToken.STR, key) :: nil =>
      currentJson match {
        case obj: Map[String, Any] =>
          obj.get(key) match {
            case Some(value) => removeKeyAndBrackets(mapToJsonString(Map(key -> value)))
            case None => throw new NoSuchElementException(s"Key '$key' not found in JSON")
          }
        case _ => throw new IllegalArgumentException("Expected a JSON object at this path")

      }
  

}
def removeKeyAndBrackets(json: String): String = {
  val withoutBrackets = json.stripPrefix("{").stripSuffix("}")
  def removeKey(string: String): String = {
    if (string.trim.startsWith(":")) {
      return string.trim.tail.strip()
    }
    removeKey(string.tail)
  }
  removeKey(withoutBrackets)
}
def getPathResult(option: String, json: Map[String, Any]): Any = {
  val tokensValues = tokenize(option)
  println(tokensValues)
  navigateRecursive(tokensValues, json)
}
