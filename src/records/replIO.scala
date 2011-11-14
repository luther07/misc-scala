package records

import scala.collection.mutable
object replIO extends StatementParser {
  def myRepl() = {
    var x : String = "";
    while(true){
      println("Please Enter a command. Type 'help;' for more information")
      x  = readLine() 
      val y = x.substring(x.length()-1, x.length())
      println(y)
      if(y != ";"){
        println("")
      }
      else{
        x match {
        case repl.help => {
          println("Interpreter help. You must type a command for an action to occur, followed by a semicolon (;)");
          println("help; brings up this screen. quit; terminates the program.");
          println("clear; will reset the store and dump; will display the contents of the store");
        }
        case repl.quit => System.exit(0)
        case repl.clear =>{
          store = Map.empty
          println("Store cleared.")
        }
        case repl.dump => println(store)
        case _ => {
          interpret(x)
        }
      } // x match
      
      }
      
    }
  }
  
  def interpret(line : String) {
    val data = parseAll(expr, line).get
    if(records.Validator.validate(data)){
      val isAssignment = data.toString().substring(0,10) == "Assignment";
      if(isAssignment){
        val variable = line.substring(0, 1)
        val mapItem = mutable.Map(variable -> Cell(0))
        store = store ++ mapItem
      }
      
      Execute(store)(data)
      println(data)
    }
    else{
      println("invalid")
    }
  }
  
  def main(args: Array[String]) {
    myRepl()
  }
  
  object repl {
  val help  = "help;";
  val quit = "quit;";
  val clear = "clear;";
  val dump = "dump;";
  val read = "read;";
}
 
  var store = Map[String, Cell](
  )
  
}