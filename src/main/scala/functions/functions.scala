
package functions
import parser.*
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
 * get
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
* existsKey
* 
* @param json The JSON data (Map[], List[], etc.).
* @param key  The key
* @return True if the key exists, false otherwise.
 */
def existsKey(json: Any, key: String): Boolean = {

  def searchKey(currentJson: Any): Boolean = currentJson match {
    case obj: Map[String, Any] =>
      if (obj.contains(key)) true
      else obj.values.exists(searchKey)

    case list: List[Any] =>
      list.exists(searchKey)

    case _ => false
  }

  searchKey(json)
}
/**
 * Delete
 *
 * @param jsonExpr The JSON data
 * @param path     The path of the element to delete
 * @return The updated JSON structure after the deletion. If the path is invalid, returns the original JSON.
 */
def delete(path: String, currentJson: Any): Any = {
  val tokens = tokenize(path)

  def deleteTokens(tokens: List[(PathToken, String)], json: Any): Any = tokens match {
    case Nil => json

    case (PathToken.DOT, _) :: (PathToken.STR, key) :: Nil =>
      json match {
        case obj: Map[String, Any] => obj - key
        case _ => json
      }

    case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: Nil =>
      json match {
        case list: List[Any] if pos.forall(_.isDigit) && pos.toInt < list.size =>
          list.take(pos.toInt) ++ list.drop(pos.toInt + 1)
        case _ => json
      }

    case (PathToken.DOT, _) :: (PathToken.STR, key) :: rest =>
      json match {
        case obj: Map[String, Any] =>
          obj.get(key).map(subJson => obj.updated(key, deleteTokens(rest, subJson))).getOrElse(json)
        case _ => json
      }

    case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: rest =>
      json match {
        case list: List[Any] if pos.forall(_.isDigit) && pos.toInt < list.size =>
          val updatedElement = deleteTokens(rest, list(pos.toInt))
          list.updated(pos.toInt, updatedElement)
        case _ => json
      }

    case _ => json
  }

  deleteTokens(tokens, currentJson)
}



/**
 * @param jsonExpr the JSON Expresion that contains the list
 * @param item    the item to be added
 * @param path  the path to a list inside of jsonExpr
 * @return a json merged (jsonExpr + other)
 */
//TODO AGREGAR MAS VERIFICACIONES PARA ITEM
def addItem(path: List[(PathToken, String)], item: Any, currentJson: Any): Any = path match {
  case Nil =>
    currentJson match {
      case list: List[Any] => list :+ item
      case _ => currentJson
    }

  case (PathToken.DOT, _) :: (PathToken.STR, pathKey) :: rest =>
    currentJson match {
      case obj: Map[String, Any] =>
        val updatedSubJson = addItem(rest, item, obj.getOrElse(pathKey, List.empty[Any]))
        obj.updated(pathKey, updatedSubJson)
      case _ => currentJson
    }

  case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: rest =>
    currentJson match {
      case list: List[Any] if pos.toInt < list.size =>
        val updatedElement = addItem(rest, item, list(pos.toInt))
        list.updated(pos.toInt, updatedElement)
      case _ => currentJson
    }

  case head :: tail =>
    val newJson = head match {
      case (PathToken.DOT, _) => currentJson
      case (PathToken.STR, key) => currentJson match {
        case obj: Map[String, Any] => obj.getOrElse(key, Map.empty)
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
    addItem(tail, item, newJson)

  case _ => currentJson
}

def map_json(json: Any, transformer: Any => Any): Any = {

  json match {
    case obj: Map[String, Any] =>
      obj.map { case (key, value) =>
        key -> transformer(value)
      }

    case list: List[Any] =>
      list.map(transformer)

    case other =>
      transformer(other)
  }
}

def addKey(path: List[(PathToken, String)], key: String, value: Any, currentJson: Any): Any = path match {
  case Nil =>
    currentJson match {
      case obj: Map[String, Any] => obj + (key -> value)
      case _ => currentJson
    }
  case (PathToken.DOT, _) :: (PathToken.STR, pathKey) :: rest =>
    currentJson match {
      case obj: Map[String, Any] =>
        val updatedSubJson = addKey(rest, key, value, obj.getOrElse(pathKey, Map.empty))
        obj.updated(pathKey, updatedSubJson)
      case _ => currentJson
    }
  case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: rest =>
    currentJson match {
      case list: List[Any] if pos.toInt < list.size =>
        val updatedElement = addKey(rest, key, value, list(pos.toInt))
        list.updated(pos.toInt, updatedElement)
      case _ => currentJson
    }
  case head :: tail =>
    val newJson = head match {
      case (PathToken.DOT, _) => currentJson
      case (PathToken.STR, key) => currentJson match {
        case obj: Map[String, Any] => obj.getOrElse(key, Map.empty)
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
    addKey(tail, key, value, newJson)

  case _ => currentJson
}

def all(json: Any, condition: Any => Boolean): Boolean = {
  json match {
    case list: List[Any] =>
      list.forall(condition)

    case _ => false
  }
}

def select_json(json: Any, condition: Any => Boolean): Any = {
  json match {
    case obj: Map[String, Any] =>
      obj.filter { case (_, value) => condition(value) }

    case list: List[Any] =>
      list.filter(condition)

    case _ =>
      if (condition(json)) json else None
  }
}
def existsKeyRec(json: Any, key: String): Boolean = {
  json match {
    case obj: Map[String, Any] =>
      obj.get(key) match {
        case Some(_) => true
        case None =>
          obj.values.exists(value => existsKeyRec(value, key))
      }

    case list: List[Any] =>
      list.exists(value => existsKeyRec(value, key))
    case _ => false
  }
}
def flatten(json: Any): List[Any] = {
  json match {
    case listOfLists: List[List[Any]] =>
      listOfLists.flatten
    case _ =>
      throw new Exception("ERR: Expected a 2D List")
  }
}