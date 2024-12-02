
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
// def addItem(path: List[(PathToken, String)], item: Any, currentJson: Any): Any = path match {
//   case Nil =>
//     currentJson match {
//       case list: List[Any] => list :+ item
//       case _ => currentJson
//     }

//   case (PathToken.DOT, _) :: (PathToken.STR, pathKey) :: rest =>
//     currentJson match {
//       case obj: Map[String, Any] =>
//         val updatedSubJson = addItem(rest, item, obj.getOrElse(pathKey, List.empty[Any]))
//         obj.updated(pathKey, updatedSubJson)
//       case _ => currentJson
//     }<  

//   case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: rest =>
//     currentJson match {
//       case list: List[Any] if pos.toInt < list.size =>
//         val updatedElement = addItem(rest, item, list(pos.toInt))
//         list.updated(pos.toInt, updatedElement)
//       case _ => currentJson
//     }

//   case head :: tail =>
//     val newJson = head match {
//       case (PathToken.DOT, _) => currentJson
//       case (PathToken.STR, key) => currentJson match {
//         case obj: Map[String, Any] => obj.getOrElse(key, Map.empty)
//         case _ => currentJson
//       }
//       case (PathToken.L_BRACE, _) => currentJson match {
//         case list: List[Any] => list
//         case _ => currentJson
//       }
//       case (PathToken.NUM, pos) => currentJson match {
//         case list: List[Any] if pos.toInt < list.size => list(pos.toInt)
//         case _ => currentJson
//       }
//       case _ => currentJson
//     }
//     addItem(tail, item, newJson)

//   case _ => currentJson
// }

def addItem(path: List[(PathToken, String)], item: Any, currentJson: Any): Any = {
  path match {
    case Nil => 
      throw new IllegalArgumentException("Error: Path cannot be empty.")

    case (PathToken.DOT, _) :: (PathToken.STR, key) :: rest =>
      currentJson match {
        case obj: Map[String, Any] =>
          val updatedSubJson = obj.get(key) match {
            case Some(subJson) => addItem(rest, item, subJson)
            case None => throw new IllegalArgumentException(s"Error: Key '$key' not found.")
          }
          obj.updated(key, updatedSubJson)
        case _ => 
          throw new IllegalArgumentException("Error: Expected an object at this level.")
      }

    case (PathToken.L_BRACE, _) :: (PathToken.NUM, pos) :: (PathToken.R_BRACE, _) :: rest =>
      currentJson match {
        case list: List[Any] =>
          val index = pos.toInt
          if (index >= 0 && index < list.size) {
            val updatedElement = addItem(rest, item, list(index))
            list.updated(index, updatedElement)
          } else {
            throw new IllegalArgumentException(s"Error: Index $index out of bounds.")
          }
        case _ => 
          throw new IllegalArgumentException("Error: Expected a list at this level.")
      }

    case _ => 
      throw new IllegalArgumentException("Error: Invalid path or structure.")
  }
}

/**
  * @brief Function that maps a JSON structure and applies a transformation function to each element.
  * @param json The JSON data (Map[], List[], etc.).
  * @param transformer function that modifies the json
  * @return Map[] or List[] modified
*/

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


/**
 * @brief Adds a key-value pair to a nested JSON-like structure based on the provided path. This function recursively traverses the path and updates the value in the JSON object or array.
 * @param path A list of tuples representing the path to the key in the structure. The path elements are 
 *             combinations of path tokens like DOT, STR, L_BRACE, NUM, etc.
 * @param key The key to be added or updated in the structure.
 * @param value The value associated with the key to be added to the structure.
 * @param currentJson The current state of the JSON-like structure (could be a Map or List).
 * @return A new JSON-like structure with the key-value pair added or updated.
 *         If the path does not match or the structure is incorrect, the original structure is returned.
 */
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


/**
 * @brief Checks if all elements in a list satisfy a given condition.
 * This function iterates over a list and applies the provided condition to each element.
 * If all elements satisfy the condition, it returns true; otherwise, it returns false.
 * @param json The input data, expected to be a List.
 * @param condition A function representing the condition to be checked for each element.
 * @return True if all elements in the list satisfy the condition, otherwise false.
 */
def all(json: Any, condition: Any => Boolean): Boolean = {
  json match {
    case list: List[Any] =>
      list.forall(condition)

    case _ => false
  }
}

/**
 * @brief Filters elements in a JSON-like structure based on a given condition.
 * This function recursively checks the elements in the provided JSON structure (Map or List).
 * If the structure is a Map, it filters key-value pairs based on the condition applied to values.
 * If the structure is a List, it filters elements based on the condition.
 * If the structure does not match a Map or List, it checks the condition for the single value.
 * @param json The input data, which can be a Map, List, or any other type.
 * @param condition A function representing the condition to be applied to each element.
 * @return A filtered version of the input JSON-like structure based on the condition.
 *         If the condition is not met, None is returned for non-iterable types.
 */

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

/**
 * @brief Checks if a key exists in a nested JSON-like structure.
 * This function recursively searches for the specified key in a Map or List. It checks if the key is directly present in a Map, 
 * or if it exists within any nested structures (Maps or Lists) within the given JSON-like structure.
 * @param json The input data, which can be a Map, List, or other types.
 * @param key The key to search for in the structure.
 * @return True if the key exists in the structure, otherwise false.
 */

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

/**
 * @brief Flattens a 2D List into a 1D List.
 * This function takes a 2D List (a list of lists) and combines all the inner lists into a single flat list.
 * If the input is not a 2D List, it throws an exception.
 * @param json The input data, expected to be a 2D List.
 * @return A 1D List containing all elements from the 2D List.
 * @throws Exception If the input is not a 2D List.
 */

def flatten(json: Any): List[Any] = {
  json match {
    case listOfLists: List[List[Any]] =>
      listOfLists.flatten
    case _ =>
      throw new Exception("ERR: Expected a 2D List")
  }
}

def edit(json: Any, tokens: List[(PathToken, String)], value: Any): Any = {

  def updateAtPath(currentJson: Any, tokens: List[(PathToken, String)]): Any = tokens match {
    case Nil => value
    case (PathToken.DOT, _) :: (PathToken.STR, key) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, idx) :: (PathToken.R_BRACE, _) :: restTokens =>
      val index = idx.toInt
      currentJson match {
        case obj: Map[String, Any] =>
          obj.get(key) match {
            case Some(list: List[Any]) if index >= 0 && index < list.size =>
              val updatedList = list.updated(index, updateAtPath(list(index), restTokens))
              obj.updated(key, updatedList)
            case Some(_) =>
              throw new Exception(s"ERR: index access")
            case None =>
              throw new Exception(s"ERR: Key not found")
          }
        case _ => throw new Exception("ERR: key[index]")
      }
    case (PathToken.DOT, _) :: (PathToken.STR, key) :: Nil =>
      currentJson match {
        case obj: Map[String, Any] =>
          obj.updated(key, value)
        case _ =>
          throw new Exception("ERR: Key access in non-object")
      }

    case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) ::
      (PathToken.NUM, idx) :: (PathToken.R_BRACE, _) :: Nil =>
      val index = idx.toInt
      currentJson match {
        case list: List[Any] if index >= 0 && index < list.size =>
          list.updated(index, value)
        case list: List[Any] =>
          throw new Exception("ERR: invalid POS")
        case _ =>
          throw new Exception("ERR: Index access in non-list")
      }

    case (PathToken.DOT, _) :: (PathToken.STR, key) ::
      (PathToken.L_BRACE, _) :: (PathToken.NUM, idx) ::
      (PathToken.R_BRACE, _) :: Nil =>
      val index = idx.toInt
      currentJson match {
        case obj: Map[String, Any] =>
          obj.get(key) match {
            case Some(list: List[Any]) if index >= 0 && index < list.size =>
              val updatedList = list.updated(index, value)
              obj.updated(key, updatedList)
            case Some(_) =>
              throw new Exception("ERR: index access")
            case None =>
              throw new Exception(s"ERR: Key not found")
          }
        case _ => throw new Exception("ERR: key[index]")
      }

    case (PathToken.DOT, _) :: (PathToken.STR, key) :: restTokens =>
      currentJson match {
        case obj: Map[String, Any] =>
          obj.get(key) match {
            case Some(nextJson) =>
              obj.updated(key, updateAtPath(nextJson, restTokens))
            case None =>
              throw new Exception(s"ERR: Key not found")
          }
        case _ => throw new Exception("ERR: access")
      }

    case (PathToken.DOT, _) :: (PathToken.L_BRACE, _) :: (PathToken.NUM, idx) :: (PathToken.R_BRACE, _) :: restTokens =>
      val index = idx.toInt
      currentJson match {
        case list: List[Any] if index >= 0 && index < list.size =>
          list.updated(index, updateAtPath(list(index), restTokens))
        case list: List[Any] =>
          throw new Exception("ERR: invalid POS")
        case _ =>
          throw new Exception("ERR: access")
      }

    case _ =>
      throw new Exception("ERR: Invalid structure")
  }

  updateAtPath(json, tokens)
}