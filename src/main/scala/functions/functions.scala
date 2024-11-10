package functions
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


def delete(tokens: List[(PathToken, String)], currentJson: Any): Any = tokens match {
  case Nil => currentJson
  case (PathToken.DOT, _) :: (PathToken.STR, key) :: Nil =>
    currentJson match {
      case obj: Map[String, Any] => obj - key
      case _ => currentJson
    }
  case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: Nil =>
    currentJson match {
      case list: List[Any] if pos.toInt < list.size =>
        val updatedList = list.take(pos.toInt) ++ list.drop(pos.toInt + 1)
        updatedList
      case _ => currentJson
    }
  case (PathToken.DOT, _) :: (PathToken.STR, key) ::  (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: Nil =>
    currentJson match {
      case obj: Map[String, Any] =>
        obj.get(key) match {
          case Some(list: List[Any]) if pos.toInt < list.size =>
            val updatedList = list.take(pos.toInt) ++ list.drop(pos.toInt + 1)
            obj.updated(key, updatedList)
          case _ => currentJson
        }
      case _ => currentJson
    }
  case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: (PathToken.DOT, _) :: (PathToken.STR, key) :: Nil =>
    currentJson match {
      case list: List[Any] if pos.toInt < list.size =>
        list(pos.toInt) match {
          case obj: Map[String, Any] =>
            val updatedObj = obj - key
            val updatedList = list.updated(pos.toInt, updatedObj)
            updatedList
          case _ => currentJson
        }
      case _ => currentJson
    }
  case (PathToken.DOT, _) :: (PathToken.STR, key) :: rest =>
    currentJson match {
      case obj: Map[String, Any] =>
        obj.get(key) match {
          case Some(subJson) =>
            val updatedSubJson = delete(rest, subJson)
            obj.updated(key, updatedSubJson)
          case None => currentJson
        }
      case _ => currentJson
    }
  case head :: tail =>
    val newJson = head match {
      case (PathToken.DOT, _) => currentJson
      case (PathToken.STR, key) => currentJson match {
        case obj: Map[String, Any] => obj.get(key).getOrElse(currentJson)
        case _ => currentJson
      }
      case (PathToken.L_BRACE, _) => currentJson match {
        case list: List[Any] => list
        case _ => currentJson
      }
      case (PathToken.NUM, pos) => currentJson match {
        case list: List[Any] if pos.toInt < list.size => list(pos.toInt)
        case _ => currentJson
      }
      case _ => currentJson
    }
    delete(tail, newJson)
  case _ => currentJson
}

def exists_key(json: Any, path: String): Boolean = {
  val keys = path.split("\\.").toList
  def searchKey(currentJson: Any, keys: List[String]): Boolean = keys match {
    case Nil => true
    case head :: tail => currentJson match {
      case obj: Map[String, Any] =>
        obj.get(head) match {
          case Some(nextJson) => searchKey(nextJson, tail)
          case None => false
        }
      case _ => false
    }
  }
  searchKey(json, keys)
}