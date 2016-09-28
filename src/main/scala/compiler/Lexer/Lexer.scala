package compiler.Lexer

object Lexer {

  def tokenize(string: String): List[TOKEN] = tokenize(string.toList)

  def tokenize(chars: List[Char]): List[TOKEN] = {
    def iter(tokens: List[TOKEN], chars: List[Char]): List[TOKEN] =
      getToken(chars) match {
        case (EOF, cs)   => (EOF :: tokens).reverse
        case (token, cs) => iter(token :: tokens, cs)
      }

    Nil
  }

  def getToken(chars: List[Char]): (TOKEN, List[Char]) = chars match {
    case Nil => (EOF, Nil)

    case c :: cs if c.isWhitespace => getToken(cs)
    case c :: cs if c.isLetter     => collectWord(c :: cs)
    case c :: cs if c.isDigit      => collectNumber(c :: cs)
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
    case id => ID(id)
  }

  def collectNumber(chars: List[Char]): (TOKEN, List[Char]) = {
    def iter(acc: String, chars: List[Char]): (TOKEN, List[Char]) =
      chars match {
        case c :: cs if c.isDigit => iter(acc + c, cs)
        case cs                   => (processNumber(acc), cs)
      }

    iter("", chars)
  }

  def processNumber(number: String): TOKEN = number match {
    case num => NUMBER(num.toInt)
  }
}
