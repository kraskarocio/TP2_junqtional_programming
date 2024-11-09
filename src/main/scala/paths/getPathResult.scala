package paths
import paths.{PathToken, extractTokens, tokenize}
import paths.PathToken.*
import mapToJson.mapToJsonString

import scala.annotation.tailrec


def navigateRecursive(tokens: List[(PathToken, String)], currentJson: Any): Any = tokens match {

  case List((PathToken.DOT, _)) => currentJson
  case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: rest =>
    currentJson match {
      case list: List[Any] if pos.toInt < list.size =>
        val nextJson = list(pos.toInt)
        if(rest.size == 0){
          return nextJson
        }
        navigateRecursive(rest, nextJson)
      case _ => None
    }
  case (PathToken.DOT, _) :: (PathToken.STR, key) :: rest =>
    currentJson match {
      case obj: Map[String, Any] =>
        obj.get(key) match {
          case Some(json) =>
            if(rest.size == 0){
              return json
            }
            navigateRecursive(rest, json)
          case None => None
        }
      case _ => None
    }
  case _ => None
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
  mapToJsonString(navigateRecursive(tokensValues, json))

}
