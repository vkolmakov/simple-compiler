package compiler.Lexer

import scala.util.{Failure, Success, Try}

object Lexer {

  def tokenize(string: String): List[TOKEN] = tokenize(string.toList)

  def tokenize(chars: List[Char]): List[TOKEN] = {

    def iter(tokens: List[TOKEN], chars: List[Char]): List[TOKEN] =
      getToken(chars) match {
        case (EOF, Nil) => (EOF :: tokens).reverse
        case (INVALID_TOKEN(token), cs) => {
          println(s"Invalid token $token")
          iter(INVALID_TOKEN(token) :: tokens, cs)
        }
        case (token, cs) => iter(token :: tokens, cs)
      }

    iter(Nil, chars)
  }

  def getToken(chars: List[Char]): (TOKEN, List[Char]) = chars match {
    case Nil                       => (EOF, Nil)
    case c :: cs if c.isWhitespace => getToken(cs)
    case c :: cs if c.isLetter     => collectWord(c :: cs)
    case c :: cs if c.isDigit      => collectNumber(c :: cs)
    case c :: cs if c == '='       => (EQUAL, cs)
    case c :: cs if c == ';'       => (SEMI, cs)
    case c :: cs if c == ','       => (COMMA, cs)
    case c :: cs                   => (INVALID_TOKEN(c.toString), cs)
  }

  def collectWord(chars: List[Char]): (TOKEN, List[Char]) = {

    def iter(acc: String, chars: List[Char]): (TOKEN, List[Char]) =
      chars match {
        case c :: cs if c.isLetterOrDigit || c == '_' => iter(acc + c, cs)
        case cs                                       => (processWord(acc), cs)
      }

    iter("", chars)
  }

  def processWord(word: String): TOKEN = word match {
    case id if id == "int" => INT_TYPE
    case id                => ID(id)
  }

  def collectNumber(chars: List[Char]): (TOKEN, List[Char]) = {

    def isValidNumberContinuation(c: Char) =
      Set(';', '+', '-', '*', '/', ',').contains(c) || c.isWhitespace

    def iter(acc: String, chars: List[Char]): (TOKEN, List[Char]) =
      chars match {
        case c :: cs if !isValidNumberContinuation(c) => iter(acc + c, cs)
        case c :: cs =>
          processNumber(acc) match {
            case Success(numberToken) => (numberToken, c :: cs)
            case Failure(_)           => (INVALID_TOKEN(acc), c :: cs)
          }
      }

    iter("", chars)
  }

  def processNumber(number: String): Try[TOKEN] = number match {
    case num => Try(NUMBER(num.toInt))
  }
}
