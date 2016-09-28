package compiler.Lexer

import scala.util.{Failure, Success, Try}

object Lexer {

  def tokenize(string: String): List[TOKEN] = tokenize(string.toList)

  val charTokens: Map[Char, TOKEN] = Map(
    '=' -> EQUAL,
    '+' -> PLUS,
    '-' -> MINUS,
    ';' -> SEMI,
    ',' -> COMMA,
    '(' -> L_PAREN,
    ')' -> R_PAREN,
    '{' -> L_CURLY,
    '}' -> R_CURLY,
    '[' -> L_BRACKET,
    ']' -> R_BRACKET
  )

  val keywords: Map[String, TOKEN] = Map(
    "int" -> INT_TYPE
  )

  def tokenize(chars: List[Char]): List[TOKEN] = {

    def iter(tokens: List[TOKEN], chars: List[Char]): List[TOKEN] =
      getToken(chars) match {
        case (EOF, Nil)                 => (EOF :: tokens).reverse
        case (INVALID_TOKEN(token), cs) => iter(INVALID_TOKEN(token) :: tokens, cs)
        case (token, cs)                => iter(token :: tokens, cs)
      }

    iter(Nil, chars)
  }

  def getToken(chars: List[Char]): (TOKEN, List[Char]) = chars match {
    case Nil                       => (EOF, Nil)
    case c :: cs if c.isWhitespace => getToken(cs)
    case c :: cs if c.isLetter     => collectWord(c :: cs)
    case c :: cs if c.isDigit      => collectNumber(c :: cs)
    case c :: cs if charTokens contains c =>
      (charTokens.getOrElse(c, INVALID_TOKEN(c.toString)), cs)
    case c :: cs => (INVALID_TOKEN(c.toString), cs)
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
    case id => keywords.getOrElse(id, ID(id))
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
