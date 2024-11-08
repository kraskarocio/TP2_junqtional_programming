package paths
import paths.{tokenize, extractTokens, PathToken}
import paths.PathToken.*
import mapToJson.mapToJsonString


def navigateRecursive(tokens: List[(PathToken, String)], currentJson: Any): Any = tokens match {

  case List((PathToken.DOT, _)) => mapToJsonString(currentJson)
  case (PathToken.DOT, _) :: (PathToken.STR, key1) :: (PathToken.DOT, _) :: (PathToken.STR, key2) :: nil =>
    currentJson match {
      case obj: Map[String, Any] =>
        obj.get(key1) match {
          case Some(innerObj: Map[String, Any]) =>
            innerObj.get(key2) match {
              case Some(value) => removeKeyAndBrackets(mapToJsonString(Map(key2 -> value)))
            }
        }
    }
  case (PathToken.DOT, _) :: (PathToken.STR, key1) :: (PathToken.PIPELINE, _) :: (PathToken.DOT, _) :: (PathToken.STR, key2) :: nil =>
    currentJson match {
      case obj: Map[String, Any] =>
        obj.get(key1) match {
          case Some(innerObj: Map[String, Any]) =>
            innerObj.get(key2) match {
              case Some(value) => removeKeyAndBrackets(mapToJsonString(Map(key2 -> value)))
            }
        }
    }
  case (PathToken.DOT, _) :: (PathToken.STR, key) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: Nil =>
    currentJson match {
      case obj: Map[String, Any] =>
        obj.get(key) match {
          case Some(innerList: List[Any]) =>
            if (pos.toInt >= 0 && pos.toInt < innerList.size) {
              val value = innerList(pos.toInt)
              removeKeyAndBrackets(mapToJsonString(Map(pos -> value)))
            }
        }
    }
  case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: (PathToken.DOT, _) :: (PathToken.STR, key) :: Nil =>
    currentJson match {
      case list: List[Any] =>
        val value = list(pos.toInt)
        removeKeyAndBrackets(mapToJsonString(value))
    }

  case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: (PathToken.PIPELINE, _) :: (PathToken.DOT, _) :: (PathToken.STR, key) :: Nil =>
    currentJson match {
      case list: List[Any] =>
        val value = list(pos.toInt)
        removeKeyAndBrackets(mapToJsonString(value))
    }


  case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: Nil =>
    currentJson match {
      case list: List[Any] =>
        val value = list(pos.toInt)
        mapToJsonString(value)
    }
  case (PathToken.DOT, _) :: (PathToken.STR, key) :: nil =>
      currentJson match {
        case obj: Map[String, Any] =>
          obj.get(key) match {
            case Some(value) => removeKeyAndBrackets(mapToJsonString(Map(key -> value)))
          }
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
def getPathResult(option: String, json: Any): Any = {
  val tokensValues = tokenize(option)
  println(tokensValues)
  navigateRecursive(tokensValues, json)
}
