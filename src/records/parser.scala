package records

import scala.util.parsing.combinator._

class StatementParser extends JavaTokenParsers {
  def expr: Parser[Statement] = (
    term ~ "+" ~ term ^^ { case l ~ _ ~ r => Plus(l, r) }
  | term ~ "-" ~ term ^^ { case l ~ _ ~ r => Minus(l, r) }
  | term
  | factor
  | statement
  | declaration
  | expression
  )
  def term: Parser[Statement] = (
    factor ~ "*" ~ factor ^^ { case l ~ _ ~ r => Times(l, r) }
  | factor ~ "/" ~ factor ^^ { case l ~ _ ~ r => Div(l, r) }
  | factor
  )
  def factor: Parser[Statement] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
  | "(" ~> expr <~ ")" ^^ { case e => e }
  )
  def expression: Parser[Statement] = (
    "new" ~> ident ^^ { case c => New(Clazz(c)) }
  | ident ~ "." ~  ident ^^ { case receiver ~ _ ~ field => Selection(Variable(receiver), field) }
  )
  def statement: Parser[Statement] = (
    ident ~ "=" ~ expr <~ ";" ^^ { case s ~ _ ~ r => Assignment(Variable(s), r) }
  | ident ~ "=" ~ ident <~ ";" ^^ { case s ~ _ ~ r => Assignment(Variable(s), Variable(r)) }
  | ident ~ "." ~ ident ~ "=" ~ expr <~ ";" ^^ { case rec ~ _ ~ field ~ _ ~ v => Assignment(Selection(Variable(rec), field), v) }
  | "while" ~ "(" ~> expr ~ ")" ~ statement ^^ { case g ~ _ ~ b => While(g, b) }
  | "{" ~> rep(statement) <~ "}" ^^ { case ss => Sequence(ss: _*) }
  )
  def declaration: Parser[Statement] = (
    "var" ~ ident <~ ";" ^^ { case _ ~ s => Variable(s) }
  )
  def struct: Parser[Clazz] = (
    "struct" ~ ident ~ "{" ~ repsep(ident, ",") ~ "}" <~ ";" ^^ { case _ ~ c ~ _ ~ l ~ _ => Clazz((c :: l.toList).toArray : _*) }  
  )
}
