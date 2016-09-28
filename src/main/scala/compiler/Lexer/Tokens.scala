package compiler.Lexer

trait TOKEN

case class ID(val id: String)     extends TOKEN
case class NUMBER(val value: Int) extends TOKEN

case object STATEMENT_TERM extends TOKEN
case object EOF            extends TOKEN

case object INT_TYPE extends TOKEN

case object EQUAL extends TOKEN
case object PLUS  extends TOKEN
case object MINUS extends TOKEN

case object L_PAREN extends TOKEN
case object R_PAREN extends TOKEN
case object L_CURLY extends TOKEN
case object R_CURLY extends TOKEN
