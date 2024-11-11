import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import parser.*


class ParserTestSpec extends AnyFunSuite {
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
    }
}
