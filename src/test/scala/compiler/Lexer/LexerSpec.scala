package compiler.Lexer

import org.scalatest._

import scala.util.Success

class LexerSpec extends WordSpec with Matchers {

  "Lexer" should {
    "tokenize declaration `int x;`" in {
      Lexer.tokenize("int x;") should contain theSameElementsInOrderAs Vector(INT_TYPE,
                                                                              ID("x"),
                                                                              SEMI,
                                                                              EOF)
    }

    "tokenize assignment `x = 0;`" in {
      Lexer.tokenize("x = 0;".toList) should contain theSameElementsInOrderAs Vector(
        ID("x"),
        EQUAL,
        NUMBER(0),
        SEMI,
        EOF)
    }

    "tokenize declaration and assignment `int tokenCount = 100;`" in {
      Lexer.tokenize("int tokenCount = 100;") should contain theSameElementsInOrderAs Vector(
        INT_TYPE,
        ID("tokenCount"),
        EQUAL,
        NUMBER(100),
        SEMI,
        EOF)
    }

    "tokenize declaration and assignment with invalid tokens `int invalidCount = 123abcd2`" in {
      Lexer.tokenize("int invalidVal = 123abcd2, x, y\n") should contain theSameElementsInOrderAs Vector(
        INT_TYPE,
        ID("invalidVal"),
        EQUAL,
        INVALID_TOKEN("123abcd2"),
        COMMA,
        ID("x"),
        COMMA,
        ID("y"),
        EOF)
    }

    "recognize a simple algebraic expression `int x = 2 + 1;`" in {
      Lexer.tokenize("int x = 2 + 1;") should contain theSameElementsInOrderAs Vector(
        INT_TYPE,
        ID("x"),
        EQUAL,
        NUMBER(2),
        PLUS,
        NUMBER(1),
        SEMI,
        EOF)
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

    "recognize `int` as INT_TYPE" in {
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
    "recognize an integer as NUMBER" in {
      Lexer.processNumber("565") should be(Success(NUMBER(565)))
    }

  }
}
