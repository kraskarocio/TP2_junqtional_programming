
package functions
import parser.JsonParser
import paths.*

/**
 * @param jsonExpr json Map[] List[]
 * @param transformer function that modifies the json
 * @return Map[] or List[] modified
 */
def mapRec(jsonExpr: Any, transformer: Any => Any): Any = jsonExpr match {
  case obj: Map[String, Any] =>
    obj.map {
      case (key, value) =>
        key -> mapRec(value, transformer)
    }
  case list: List[Any] =>
    list.map(item => mapRec(item, transformer))
  case other => transformer(other)
}

/**
 *
 * @param jsonExpr json Map[] List[]
 * @param level
 * @return the elements on that level
 */
def depth(jsonExpr: Any, level: Int): List[Any] = {
  def depthHelper(expr: Any, currentLevel: Int): List[Any] = {
    if (currentLevel == level) {
      expr match {
        case map: Map[_, _] => map.values.toList
        case list: List[_] => list
        case other => List(other)
      }
    } else {
      expr match {
        case map: Map[_, _] =>
          map.values.toList.flatMap(value => depthHelper(value, currentLevel + 1))
        case list: List[_] =>
          list.flatMap(item => depthHelper(item, currentLevel + 1))
        case _ => List()
      }
    }
  }

  depthHelper(jsonExpr, 0)
}

/**
 * to use this function typeof jsonExpr == other
 * @param jsonExpr json Map[] List[] (json to merge into)
 * @param other json Map[] List[] (json to merge)
 * @return a json merged (jsonExpr + other)
 */
def merge(jsonExpr: Any, other: Any): Any = (jsonExpr, other) match {
  case (list1: List[Any], list2: List[Any]) =>
    list1 ++ list2

  case (map1: Map[String, Any], map2: Map[String, Any]) =>
    map2.foldLeft(map1) { case (acc, (key, value)) =>
      if (acc.contains(key)) acc else acc + (key -> value)
    }

  case _ =>
    throw new IllegalArgumentException("Type of 'other' != 'jsonExpr'")
}

/**
 *
 * @param jsonExpr The JSON data (Map[], List[], etc.).
 * @param path     The path to navigate through the JSON.
 * @return The value at the specified path, or null if not found.
 */
def get(currentJson: Any, path: String): Any = {
  val tokens = tokenize(path)
  navigateRecursive(tokens, currentJson)
}


/**
*
* @param jsonExpr The JSON data (Map[], List[], etc.).
* @param path     The path to check for existence.
* @return True if the key exists, false otherwise.
 */

def existsKey(currentJson: Any, path: String): Boolean = {
  val tokens = tokenize(path)
  def searchRecursive(tokens: List[(PathToken, String)], currentJson: Any): Boolean = tokens match {
    case Nil => true

    //acceso .[index]
    case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, idx) :: (PathToken.R_BRACE, _) :: rest =>
      currentJson match {
        case list: List[Any] if idx.forall(_.isDigit) =>
          val index = idx.toInt
          if (index >= 0 && index < list.size) {
            searchRecursive(rest, list(index))
          } else {
            false
          }
        case _ => false
      }

    // acceso .key
    case (PathToken.DOT, _) :: (PathToken.STR, key) :: rest =>
      currentJson match {
        case obj: Map[String, Any] =>
          obj.get(key) match {
            case Some(nextJson) => searchRecursive(rest, nextJson)
            case None => false
          }
        case _ => false
      }
    case _ => false
  }
  searchRecursive(tokens, currentJson)
}
