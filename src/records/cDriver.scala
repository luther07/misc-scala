package records

import java.io.FileReader

/* When executed with an argument that is the name of a file,
   this main method will print the result of parsing the file.
   Printing the result of parsing a file is useful only for 
   debugging. */ 

/* This program can quickly and easily test the parser/lexer against a source file.
   I use sbt's 'run' command and a filename argument, then choose the corresponding
   class containing a main method, from the provided menu. 
   For example, from sbt's prompt type 'run testfile.txt', then choose 'records.CParse' when
   asked which main class you want to run. */

object CParse extends StatementParser {
  def main(args: Array[String]) {
    val reader = new FileReader(args(0))
    val data = parseAll(expr, reader).get
    if (records.Validator.validate(data))
      println(data)
    else
      println("invalid")
  }
}
