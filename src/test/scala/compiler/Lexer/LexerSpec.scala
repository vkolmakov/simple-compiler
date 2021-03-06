package compiler.Lexer

import org.scalatest._

import scala.util.{Failure, Success}

class LexerSpec extends WordSpec with Matchers {

  "Lexer" should {
    "tokenize declaration `int x;`" in {
      val expected = Vector(INT_TYPE, ID("x"), SEMI, EOF)

      Lexer.tokenize("int x;") should contain theSameElementsInOrderAs expected
    }

    "tokenize assignment `x = 0;`" in {
      val expected = Vector(ID("x"), EQUAL, NUMBER(0), SEMI, EOF)

      Lexer.tokenize("x = 0;".toList) should contain theSameElementsInOrderAs expected
    }

    "tokenize declaration and assignment `int tokenCount = 100;`" in {
      val expected = Vector(INT_TYPE, ID("tokenCount"), EQUAL, NUMBER(100), SEMI, EOF)

      Lexer.tokenize("int tokenCount = 100;") should contain theSameElementsInOrderAs expected
    }

    "tokenize declaration and assignment with invalid tokens `int invalidCount = 123abcd2`" in {
      val expected = Vector(INT_TYPE,
                            ID("invalidVal"),
                            EQUAL,
                            INVALID_TOKEN("123abcd2"),
                            COMMA,
                            ID("x"),
                            COMMA,
                            ID("y"),
                            EOF)

      Lexer.tokenize("int invalidVal = 123abcd2, x, y\n") should contain theSameElementsInOrderAs expected
    }

    "recognize a simple algebraic expression `int x = 2 + 1;`" in {
      val expected =
        Vector(INT_TYPE, ID("x"), EQUAL, NUMBER(2), PLUS, NUMBER(1), SEMI, EOF)

      Lexer.tokenize("int x = 2 + 1;") should contain theSameElementsInOrderAs expected
    }
    "be whitespace independent and tokenize `int sum=-22+1;`, ` int sum=-22 +1 ;` and `\\t\\tint\\nsum=  - 22+1\\n;" in {
      val expected =
        Vector(INT_TYPE, ID("sum"), EQUAL, MINUS, NUMBER(22), PLUS, NUMBER(1), SEMI, EOF)

      Lexer.tokenize("int sum=-22+1;") should contain theSameElementsInOrderAs expected
      Lexer.tokenize(" int sum=-22 +1 ;") should contain theSameElementsInOrderAs expected
      Lexer.tokenize("\t\tint\nsum=  - 22+1\n;") should contain theSameElementsInOrderAs expected
    }

  }

  "getToken" should {
    "recognize end-of-file" in {
      Lexer.getToken(Nil) should be(EOF, Nil)
    }

    "ignore whitespace" in {
      Lexer.getToken("     \t   \n  ".toList) should be(EOF, Nil)
      Lexer.getToken(" \t someId".toList) should be(ID("someId"), Nil)
    }

    "recognize special symbols" in {
      Lexer.getToken(";\n".toList) should be(SEMI, "\n".toList)
      Lexer.getToken("=25".toList) should be(EQUAL, "25".toList)
      Lexer.getToken(", xyz".toList) should be(COMMA, " xyz".toList)
      Lexer.getToken("- xyz".toList) should be(MINUS, " xyz".toList)
      Lexer.getToken("+ xyz".toList) should be(PLUS, " xyz".toList)
      Lexer.getToken("(xyz".toList) should be(L_PAREN, "xyz".toList)
      Lexer.getToken(")xyz".toList) should be(R_PAREN, "xyz".toList)
      Lexer.getToken("{xyz".toList) should be(L_CURLY, "xyz".toList)
      Lexer.getToken("}xyz".toList) should be(R_CURLY, "xyz".toList)
      Lexer.getToken("[xyz".toList) should be(L_BRACKET, "xyz".toList)
      Lexer.getToken("]xyz".toList) should be(R_BRACKET, "xyz".toList)
    }
  }

  "collectWord" should {
    "be able to recognize an arbitrary string as a variable name" in {
      Lexer.collectWord("redFish;\n".toList) should be(ID("redFish"), ";\n".toList)
    }

    "allow underscores in variable names" in {
      Lexer.collectWord("DATE_FMT = \"%y%m%d\";\n".toList) should be(
        ID("DATE_FMT"),
        " = \"%y%m%d\";\n".toList)
    }

    "allow for numbers in variable names" in {
      Lexer.collectWord("myList123 = [];\n".toList) should be(ID("myList123"),
                                                              " = [];\n".toList)
    }
  }

  "processWord" should {
    "identify and arbitrary string as ID" in {
      Lexer.processWord("greenWhale") should be(ID("greenWhale"))
    }

    "recognize all keywords as INT_TYPE" in {
      Lexer.processWord("int") should be(INT_TYPE)
    }
  }

  "collectNumber" should {
    "recognize an integer as NUMBER" in {
      Lexer.collectNumber("1234;\n".toList) should be(NUMBER(1234), ";\n".toList)
    }

    "recognize invalid integers as INVALID_TOKEN" in {
      Lexer.collectNumber("123hello;\n".toList) should be(INVALID_TOKEN("123hello"),
                                                          ";\n".toList)
    }
  }

  "processNumber" should {
    "recognize a valid integer string as NUMBER" in {
      Lexer.processNumber("565") should be(Success(NUMBER(565)))
    }

    "fail on invalid integer string" in {
      Lexer.processNumber("321abc") shouldBe a[Failure[_]]
      Lexer.processNumber("321_abc") shouldBe a[Failure[_]]
      Lexer.processNumber("12-3") shouldBe a[Failure[_]]
    }

  }
}
