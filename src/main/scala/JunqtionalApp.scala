import scala.io.Source
import parser.JsonParser.*
import paths.*
import mapToJson.*

object JunqtionalApp {
  def get(json: Map[String, Any], path: String): Any = {
    val tokens = tokenize(path)
    navigateRecursive(tokens, json) match {
      case None => null
      case result => result
    }
  }

  def delete(json: Map[String, Any], path: String): Map[String, Any] = {
    val keys = path.split("\\.").toList

    def searchToDelete(currentJson: Any, keys: List[String]): Any = keys match {
      case Nil => currentJson
      case lastKey :: Nil => currentJson match {
        case obj: Map[String, Any] =>
          obj - lastKey
        case _ => currentJson
      }
      case head :: tail => currentJson match {
        case obj: Map[String, Any] =>
          obj.get(head) match {
            case Some(nextJson) =>
              val updatedChild = searchToDelete(nextJson, tail)
              obj.updated(head, updatedChild)
            case None => obj
          }
        case list: List[Any] =>
          if (head.forall(_.isDigit)) {
            val index = head.toInt
            if (index >= 0 && index < list.length) {
              val updatedList = list.patch(index, Nil, 1)
              updatedList
            } else {
              list
            }
          } else {
            list
          }
        case _ => currentJson
      }
    }
    val updatedJson = searchToDelete(json, keys)

    updatedJson match {
      case updatedMap: Map[String, Any] => updatedMap
      case _ => json
    }
  }

  def exists_key(json: Map[String, Any], path: String): Boolean = {
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

  def main(args: Array[String]): Unit = {
    try {
      val option = ".a.b.c.d.[1]"
      val pathFile = "resources/EX1.json"
      val input = Source.fromFile(pathFile).getLines().mkString
      val mapJson = jsonParser(input)
      println(" - - - - - MAP - - - - -")
      println(mapJson)

      var pathRes: Any = null
      if(option.startsWith(".")){
        pathRes = getPathResult(option, mapJson)
      }
      println("--- PATH(res) ---")
      println(pathRes)


      println("\n\nPRUEBA DE GET:")
      val path1 = "address"
      val jsonResult1 = get(mapJson.asInstanceOf[Map[String, Any]], path1)
      //lo hice map porq jsonParse e devuelve any
      println(s"Result for path '$path1': $jsonResult1")
      val path = ".a.b.c.d.[0]"
      val jsonResult = get(mapJson.asInstanceOf[Map[String, Any]], path)
      println(s"Result for path '$path': $jsonResult")

      println("\n\n--- PRUEBA DE DELETE:")
      println("JSON Original:")
      println(mapToJsonString(mapJson))
      val updatedJson = delete(mapJson.asInstanceOf[Map[String, Any]], "a.b.c.d.[0]")
      println("Después de eliminar 'pos0 de d':")
      println(mapToJsonString(updatedJson))

      println("\n\nPRUEBA DE EXISTS:")
      val keyExists1 = exists_key(mapJson.asInstanceOf[Map[String, Any]], "city")
      println(s"Key 'city' exists: $keyExists1")
      val keyExists2 = exists_key(mapJson.asInstanceOf[Map[String, Any]], "a.b.c.d")
      println(s"Key 'd' exists: $keyExists2")

    } catch {
      case e: Exception => println(s"Error: ${e.getMessage}")
    }
  }
}
