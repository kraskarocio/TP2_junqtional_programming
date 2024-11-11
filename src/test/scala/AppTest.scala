import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import parser.*
import paths.*


class AppTestSpec extends AnyFunSuite {
    test("Parse simple dictionary") {
        assert(jsonParser("""{"a":0}""") == Map("a" -> 0))
    }
    test("Parse simple list") {
        assert(jsonParser("[1,2,3]") == List(1, 2, 3))
    }
    test("Parse complex list") {
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
    test("Path simple list") {
        val json = jsonParser("[1,2,3]")
        val tokens = tokenize(".[0]")
        assert(navigateRecursive(tokens, json) == 1)
    }
    test("Path simple dictionary") {
        val json = jsonParser("""{"a":0}""")
        val tokens = tokenize(".a")
        assert(navigateRecursive(tokens, json) == 0)
    }
    test("Path complex list") {
        val pathFile = "resources/exListMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val tokens = tokenize(".[1].hola")
        assert(navigateRecursive(tokens, json) == "mundo")
    }
    test("Path complex dictionary") {
        val pathFile = "resources/exListMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val tokens = tokenize(".[4].a.b.c.d")
        assert(navigateRecursive(tokens, json) == 1)
    }
    test("Path dictionary list") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val tokens = tokenize(".hola[0]")
        assert(navigateRecursive(tokens, json) == 1)
    }
    test("Path dictionary list complex") {
        val pathFile = "resources/exMap.json"
        val input = Source.fromFile(pathFile).getLines().mkString
        val json = jsonParser(input)
        val tokens = tokenize(".hola[1].s.a[1]")
        assert(navigateRecursive(tokens, json) == 2)
    }
}
