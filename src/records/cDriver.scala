package records

import java.io.FileReader

/* When executed with an argument that is the name of a file,
   this main method will print the result of parsing the file.
   Printing the result of parsing a file is useful only for 
   debugging. */ 

object CParse extends StatementParser {
  def main(args: Array[String]) {
    val reader = new FileReader(args(0))
    println(parseAll(expr, reader))
  }
}
