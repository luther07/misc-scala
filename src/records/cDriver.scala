package records

import java.io.FileReader

object CParse extends StatementParser {
  def main(args: Array[String]) {
    val reader = new FileReader(args(0))
    println(parseAll(expr, reader))
  }
}
