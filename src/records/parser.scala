package records

import scala.util.parsing.combinator._

object StatementParser extends JavaTokenParsers {
  def expr: Parser[Statement] = (
    term ~ "+" ~ term ^^ { case l ~ _ ~ r => Plus(l, r) }
  | term ~ "-" ~ term ^^ { case l ~ _ ~ r => Minus(l, r) }
  | term
  | factor
  )
  def term: Parser[Statement] = (
    factor ~ "*" ~ factor ^^ { case l ~ _ ~ r => Times(l, r) }
  | factor ~ "/" ~ factor ^^ { case l ~ _ ~ r => Div(l, r) }
  | factor
  )
  def factor: Parser[Statement] = (
    wholeNumber ^^ { case s => Constant(s.toInt) }
  | "var" ~ ident ^^ { case _ ~ s => Variable(s) }
  | "(" ~> expr <~ ")" ^^ { case e => e }
  )
  def statement: Parser[Statement] = (
    ident ~ "=" ~ expr ^^ { case s ~ _ ~ r => Assignment(Variable(s), r) }
  | "while" ~ "(" ~> expr ~ ")" ~ statement ^^ { case g ~ _ ~ b => While(g, b) }
  | "{" ~> repsep(statement, ",") <~ "}" ^^ { case ss => Sequence(ss: _*) }
  | "new" ~> ident ^^ { case c => New(Clazz(c))}
  )
  def struct: Parser[Clazz] = (
    "struct" ~ ident ~ "{" ~ repsep(ident, ",") ~ "}" ^^ { case _ ~ c ~ _ ~ l ~ _ => Clazz((c :: l.toList).toArray : _*) }  
  )
}
