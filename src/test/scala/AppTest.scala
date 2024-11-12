import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import parser.*
import paths.*
import functions.*

class AppTestSpec extends AnyFunSuite {
    test("PARSE: simple dictionary") {
        assert(jsonParser("""{"a":0}""") == Map("a" -> 0))
    }
    test("PARSE: simple list") {
        assert(jsonParser("[1,2,3]") == List(1, 2, 3))
    }
    test("PARSE: complex list") {
        val pathFile = "resources/exListMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        assert(jsonParser(input) == List(1, 
        Map("hola" -> "mundo"), "Hola Mundo", List( 11, 22, 0), 
        Map("a" -> Map("b" -> Map("c" -> Map("d" -> 1))))))
    }
    test("Invalid json cannot be parsed (invalid obj/expr)") {
        assertThrows[Exception] {
            jsonParser("[1,")
        }        
        assertThrows[Exception] {
            jsonParser("[1")
        }
        assertThrows[Exception] {
            jsonParser("""{"s":}""")
        }
        assertThrows[Exception] {
            jsonParser("{hola : no}")
        }
        assertThrows[Exception] {
            jsonParser("{1 : 2}")
        }        
        assertThrows[Exception] {
            jsonParser("""{"a": 1 "b": 2}""")
        }
    }
    test("PATH: simple list") {
        val json = jsonParser("[1,2,3]")
        val tokens = tokenize(".[0]")
        assert(navigateRecursive(tokens, json) == 1)
    }
    test("PATH: simple dictionary") {
        val json = jsonParser("""{"a":0}""")
        val tokens = tokenize(".a")
        assert(navigateRecursive(tokens, json) == 0)
    }
    test("PATH: complex list") {
        val pathFile = "resources/exListMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val tokens = tokenize(".[1].hola")
        assert(navigateRecursive(tokens, json) == "mundo")
    }
    test("PATH: complex dictionary") {
        val pathFile = "resources/exListMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val tokens = tokenize(".[4].a.b.c.d")
        assert(navigateRecursive(tokens, json) == 1)
    }
    test("PATH: dictionary list") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val tokens = tokenize(".hola[0]")
        assert(navigateRecursive(tokens, json) == 1)
    }
    test("PATH: dictionary list complex") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val tokens = tokenize(".hola[1].s.a[0]")
        assert(navigateRecursive(tokens, json) == List(1,2))
    }
    test("FUNC: [existsKey] 1st level") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        assert(existsKey(json, "hola") == true)
    }
    test("FUNC: [existsKey] 2nd level") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        assert(existsKey(json, "a"))
    }
    test("FUNC: [existsKey] 3rd level") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        assert(existsKey(json, "s"))
    }
    test("FUNC: [existsKey] not key") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        assert(!existsKey(json, "g"))
    }
    test("FUNC: [merge] map") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val newJson = jsonParser("""{"chau":1}""")
        val res = merge(json,newJson)
        assert(existsKey(res, "chau"))
        assert(existsKey(res, "hola"))
        assert(existsKey(res, "a"))
        assert(existsKey(res, "s"))
        val tokens = tokenize(".chau")
        assert(navigateRecursive(tokens, res) == 1)
    }
    test("FUNC: [merge] list") {
        val pathFile = "resources/exListMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val newJson = jsonParser("""[90]""")
        val res = merge(json, newJson)
        assert(existsKey(res, "b"))
        assert(existsKey(res, "hola"))
        assert(existsKey(res, "a"))
        assert(existsKey(res, "c"))
        assert(existsKey(res, "d"))
        val myValue = res.asInstanceOf[List[Any]]
        assert(myValue.contains(90))
    }
    test("FUNC: [delete] simple Map") {
        val json = jsonParser("""{ "a": { "b" : 1 } }""")
        val delJson = delete(".a.b", json)
        assert(!existsKey(delJson, "b"))
    }
    test("FUNC: [delete] simple List") {
        val json = jsonParser("""[{"a": 1}, 0 , 2]""")
        val delJson = delete(".[0].a", json)
        assert(!existsKey(delJson, "a"))
    }
    test("FUNC: [depth] 1"){
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val depJson = depth(json, 1)
        assert(depJson.contains(1))
        assert(depJson.contains("g"))
    }
    test("FUNC: [depth] 2"){
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val depJson = depth(json, 2)
        assert(depJson.contains(8))
        assert(depJson.contains("g"))
    }
    test("FUNC: [depth] 4") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val depJson = depth(json, 4)
        assert(depJson.contains(4))
    }
    test("FUNC: [depth] 5") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val depJson = depth(json, 5)
        assert(depJson.contains(1))
    }
    test("FUNC: [map_rec]"){
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val recJson = mapRec(json, sum1)
        val tokens = tokenize(".hola[0]")
        assert(navigateRecursive(tokens, recJson) == 2)
    }
}
