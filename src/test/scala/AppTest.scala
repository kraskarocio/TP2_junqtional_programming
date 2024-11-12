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
        test("FUNC: [add_key] map"){
        val json = jsonParser("""{"a":{"b":1}}""")
        val token = tokenize(".a")
        val addJson = addKey(token, "c", 1, json)
        assert(existsKey(addJson, "c"))
    }
    test("FUNC: [add_key]  List(Map())") {
        val json = jsonParser("""[1,2,{"a":0}]""")
        val token = tokenize(".[2]")
        val addJson = addKey(token, "c", 1, json)
        val depJson = depth(addJson, 1)
        assert(existsKey(addJson, "c"))
    }
    test("FUNC: [add_item] list"){
        val json = jsonParser("""[1,2,{"a":0}]""")
        val token = tokenize(".")
        val addJson = addItem(token, List(1,90),json)
        val depJson = depth(addJson, 2)
        println(addJson)
        assert(depJson.contains(90))
    }
    test("FUNC: [add_item] Map(List())") {
        val json = jsonParser("""{"a":[1,2]}""")
        val token = tokenize(".a")
        val addJson = addItem(token, List(1, 90), json)
        val depJson = depth(addJson, 2)
        println(addJson)
        assert(depJson.contains(2))
    }
    test("FUNC: [mapJson] map") {
        val json = jsonParser("""{"a":{"v":{"b":0}}, "bool1": true, "bool2": true}""")
        val mapJson = map_json(json, negateBooleans)
        val token = tokenize(".bool1")
        val res = navigateRecursive(token, mapJson)
        assert(res == false)
        assert(existsKey(mapJson, "a"))
    }
    test("FUNC: [mapJson] list") {
        val json = jsonParser("""[true, true, {"a":false}]""")
        val mapJson = map_json(json, negateBooleans)
        val token = tokenize(".[2].a")
        val res = navigateRecursive(token, mapJson)
        val token2 = tokenize(".[1]")
        val res2 = navigateRecursive(token2, mapJson)
        assert(res == false)
        assert(res2 == false)
    }
    test("FUNC: [select] map") {
        val json = jsonParser("""{"a": true, "b": false}""")
        val selectJson = select_json(json, isTrue)
        assert(existsKey(selectJson, "a"))
        assert(!existsKey(selectJson, "b"))
    }
    test("FUNC: [select] list") {
        val json = jsonParser("""[1,2,90,50, true]""")
        val selectJson = select_json(json, greaterThan10)
        val resSelectJson = selectJson.asInstanceOf[List[Any]]
        assert(!resSelectJson.contains(true))
        assert(resSelectJson.contains(90))
        assert(!resSelectJson.contains(1))
    }
    test("FUNC: [exists_key_rec] Map") {
        val json = jsonParser("""{"hola":{"todo":{"bien":"?"}}}""")
        assert(!existsKeyRec(json, "?"))
        assert(existsKeyRec(json, "todo"))
        assert(existsKeyRec(json, "bien"))
        assert(existsKeyRec(json, "hola"))
    }
    test("FUNC: [exists_key_rec] List(Map())") {
        val json = jsonParser("""[1,2,"hola", {"a":{"b":["c", {"d":"e"}]}}]""")
        assert(!existsKeyRec(json, "e"))
        assert(!existsKeyRec(json, "hola"))
        assert(!existsKeyRec(json, "c"))
        assert(existsKeyRec(json, "a"))
        assert(existsKeyRec(json, "b"))
    }
    test("FUNC: [all] correct"){
        val json = jsonParser("""[20,30,40,100]""")
        assert(all(json, greaterThan10))
    }
    test("FUNC: [all] incorrect"){
        val json = jsonParser("""[20,30,true]""")
        assert(!all(json, greaterThan10))
    }
    test("FUNC: [all] doesn't work with Map()"){
        val json = jsonParser("""{"a": 20, "b": 40}""")
        assert(!all(json, greaterThan10))
    }
    test("FUNC: [flatter]") {
        val json = jsonParser("""[[1,2],[3,4],[5,6]]""")
        assert(flatten(json)== List(1,2,3,4,5,6))
    }
    test("FUNC: [flatter] List(List(...)List(...))") {
        val json = jsonParser("""[[{"a":0},{"b":1}],[{"c":2},{"e":1}]]""")
        assert(flatten(json) == List(Map("a"->0),Map("b"->1), Map("c"-> 2), Map("e"->1)))
    }
    test("FUNC: [edit] List") {
        val json = jsonParser("""[[1,2],2,{"a":0}]""")
        val editJson = edit(json,"hola", ".")
        val edited = editJson.asInstanceOf[List[Any]]
        assert(edited.contains("hola"))
    }
    test("FUNC: [edit] ") {
        val json = jsonParser("""{"chao":[1,2]}""")
        val editJson = edit(json, "hola", ".chao")
        val token = tokenize(".chao")
        val aux = navigateRecursive(token, editJson)
        val aux1 = aux.asInstanceOf[List[Any]]
        assert(aux1.contains("hello"))
    }
}
