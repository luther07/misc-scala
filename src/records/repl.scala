package records

object repl {
  val help = "help;";
  val quit = "quit;";
  val clear = "clear;";
  val dump = "dump;";
}
 
class replRunner() {
  def myRepl() {
    while(true){
      println("Please Enter a command. Type 'help;' for more information")
      val x = readLine() match {
        case repl.help => "";
        case repl.quit => "";
        case repl.clear => "";
        case repl.dump => "";
        case _ => "";
      }
    }
  }
}
