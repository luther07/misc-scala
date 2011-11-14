package records

object replIO {
  def myRepl() {
    while(true){
      println("Please Enter a command. Type 'help;' for more information")
      val x = readLine() match {
        case repl.help => {
          println("Interpreter help. You must type a command for an action to occur, followed by a semicolon (;)");
          println("help; brings up this screen. quit; terminates the program.");
          println("clear; will reset the store and dump; will display the contents of the store")
        }
        case repl.quit => System.exit(0)
        case repl.clear => "";
        case repl.dump => "";
        case _ => {
          println("You selected an incorrect key... Please select again.");
          myRepl();
        }
      }
    }
  }
  
  def main(args: Array[String]) {
    myRepl()
  }
  
  object repl {
  val help = "help;";
  val quit = "quit;";
  val clear = "clear;";
  val dump = "dump;";
}
}