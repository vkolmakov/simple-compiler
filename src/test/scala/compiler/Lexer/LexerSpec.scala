package compiler.Lexer

import org.scalatest._

class LexerSpec extends WordSpec with Matchers {

  "Lexer" should {
    "be able to tokenize \"int x;\"" in {
      Lexer.tokenize("int x;") should contain theSameElementsAs Vector(INT_TYPE,
                                                                       ID("x"),
                                                                       STATEMENT_TERM,
                                                                       EOF)
    }
  }

  "getToken" should {
    "recognize end-of-file" in {
      Lexer.getToken(Nil) should be(EOF, Nil)
    }

    "ignore whitespace" in {
      Lexer.getToken("     \t   \n  ".toList) should be(EOF, Nil)
    }

    "recognize ';' as statement delimiter" in {
      Lexer.getToken(";\n".toList) should be(STATEMENT_TERM, "\n".toList)
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
  }

  "processWord" should {
    "identify and arbitrary string as ID" in {
      Lexer.processWord("greenWhale") should be(ID("greenWhale"))
    }

    "recognize \"int\" as INT_TYPE" in {
      Lexer.processWord("int") should be(INT_TYPE)
    }
  }

  "collectNumber" should {
    "recognize an integer as NUMBER" in {
      Lexer.collectNumber("1234;\n".toList) should be(NUMBER(1234), ";\n".toList)
    }
  }

  "processNumber" should {
    "recognize an integer as NUMBER" in {
      Lexer.processNumber("565") should be(NUMBER(565))
    }
  }
}
