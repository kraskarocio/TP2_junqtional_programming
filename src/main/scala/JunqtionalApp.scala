
import io.circe.*
import io.circe.parser.*
import io.circe.syntax.*
import io.circe.generic.auto.*
import scala.io.Source
import java.io.*

object JunqtionalApp {
  def printJson(json: Json): Unit = {
    println(json)
  }

  def saveJsonToFile(json: Json, filePath: String): Unit = {
    val fileWriter = new PrintWriter(new File(filePath))
    try {
      fileWriter.write(json.spaces2)
    } finally {
      fileWriter.close()
    }
  }
  def get(json: Json, path: String): Json = {
    val keys = path.split("\\.").toList
    def getSearch(currentJson: Json, keys: List[String]): Json = keys match {
      case Nil => currentJson
      case head :: tail =>
        currentJson.asObject match {
          case Some(obj) =>
            obj.toMap.get(head) match {
              case Some(nextJson) => getSearch(nextJson, tail)
              case None => Json.Null
            }
          case _ => Json.Null
        }
    }
    getSearch(json, keys)
  }
  def addKey(json: Json, path: String, key: String, value: Json): Json = {
    val keys = path.split("\\.").toList
    def addAtPath(currentJson: Json, keys: List[String]): Json = keys match {
      case Nil => currentJson
      case lastKey :: Nil =>
        currentJson.asObject match {
          case Some(obj) => obj.add(key, value).asJson
          case _ => currentJson
        }
      case head :: tail =>
        currentJson.asObject match {
          case Some(obj) =>
            val nextJson = obj.toMap.getOrElse(head, Json.obj())
            val updatedSon = addAtPath(nextJson, tail)
            obj.add(head, updatedSon).asJson
        }
    }
    addAtPath(json, keys)
  }

  def delete(json: Json, path: String): Json ={
    val keys = path.split("\\.").toList
    def searchToDelete(currentJson: Json, keys: List[String]): Json = keys match {
      case Nil => currentJson
      case lastKey :: Nil =>
        currentJson.asObject match {
          case Some(obj) => Json.fromJsonObject(obj.remove(lastKey))
          case None => currentJson
        }
      case head :: tail =>
        currentJson.asObject match {
          case Some(obj) =>
            obj.toMap.get(head) match {
              case Some(nextJson) =>
                val updatedSon = searchToDelete(nextJson, tail)
                Json.fromJsonObject(obj.add(head, updatedSon))
              case None => currentJson
            }
          case None => currentJson
        }
    }
  searchToDelete(json, keys)
  }
  def exists_key(json: Json, key: String): Boolean = {
    def searchKey(currentJson: Json): Boolean = {
      currentJson.asObject match {
        case Some(obj) =>
          obj.toMap.exists {
            case (k, v) if k == key => true
            case (_, v) => searchKey(v)
          }
        case _ => false
      }
    }
    searchKey(json)
  }
  def main(args: Array[String]): Unit = {
// Pongo esto para correrlo desde el Intellij y no mandando arg desde el termina
    val func = "print"
    val path = "resources/EX1.json"
    /*
    if (args.isEmpty) {
      println("ERR: No arguments provided")
      return
    }
    val func = args(0) // function to run
    val path = args(1) // path to JSON file
          // read JsonFile

    */

    val jsonString = try {
      Source.fromFile(path).mkString
    } catch {
      case e: Exception =>
        println(s"Error reading the file: ${e.getMessage}")
        return
    }
    val json: Either[ParsingFailure, Json] = parse(jsonString)
    json match {
      case Right(parsedJson) =>

  //* Esto es para ir chequeando si andan las funciones. Desp sacarlo


        var updatedJson = addKey(parsedJson, "usuario.direccion", "activo", Json.fromBoolean(true))
        updatedJson = addKey(updatedJson, "usuarioDoble.nombre", "TareasCompletadas", Json.fromInt(5))
        updatedJson = delete(updatedJson, "usuario.edad")
        saveJsonToFile(updatedJson, path)
        println(get(updatedJson, "usuarioTriple.nombre"))
        println(exists_key(updatedJson, "TareasCompletadas"))
  // Hasta aca

        // if the JSON is valid, choose which function to run based on the first argument
        func match {
          case "print" => printJson(updatedJson) // Add more functions here ...
  //esto tambien borrarlo
            updatedJson = addKey(updatedJson, "usuario.nombre", "edad", Json.fromInt(5))
            updatedJson = delete(updatedJson, "usuarioDoble.TareasCompletadas")
            updatedJson = delete(updatedJson, "usuario.activo")
            saveJsonToFile(updatedJson, path)

  // Hasta aca
          case _ => println(s"ERR: Unknown function '$func'")
        }
      case Left(error) =>
        println(s"Error parsing the JSON: $error")
    }
  }
}
