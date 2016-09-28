package compiler.Lexer

import org.scalatest._

class LexerSpec extends WordSpec with Matchers {

  "Lexer" should {
    "be able to tokenize \"int x;\"" in {
      Lexer.tokenize("int x;") should contain theSameElementsAs Vector(INT_TYPE, ID("x"), STATEMENT_TERM)
    }
  }

  "getToken" should {
    "recognize end-of-file" in {
      Lexer.getToken(Nil) should be(EOF, Nil)
    }

    "ignore whitespace" in {
      Lexer.getToken("     \t   \n  ".toList) should be(EOF, Nil)
    }
  }

  "processWord" should {
    "be able to recognize an arbitrary string as a variable name" in {
      Lexer.processWord("redFish;\n".toList) should be(ID("redFish"), ";\n".toList)
    }

    "allow underscores in variable names" in {
      Lexer.processWord("DATE_FMT = \"%y%m%d\";\n".toList) should be(ID("DATE_FMT"), " = \"%y%m%d\";\n".toList)
    }
  }

  "matchWord" should {
    "identify and arbitrary string as ID" in {
      Lexer.matchWord("greenWhale") should be(ID("greenWhale"))
    }

  }

}
