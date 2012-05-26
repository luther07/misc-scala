package records

/*
 * This object extends StatementParser in order to test the parser/lexer.
 * 
 */

object  ParseLib extends StatementParser {
  def parsestatement(str: String): Statement = {
    parse(expr, str).get
  }
}
