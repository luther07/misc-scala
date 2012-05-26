package records

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class BDDTests extends FlatSpec with ShouldMatchers {

  "A constant" should
     "spit out its abstract syntax" in {
    val aConst = records.ParseLib.parsestatement("1")
    aConst.toString() should be ("Constant(1)")
  }

  "A variable" should
     "spit out its abstract syntax" in {
    val aVar = records.ParseLib.parsestatement("var x;")
    aVar.toString() should be ("Variable(x)")
  }

  "An assignment statement" should
     "spit out its abstract syntax" in {
    val anAssignment = records.ParseLib.parsestatement("x = 5;")
    anAssignment.toString() should  be ("Assignment(Variable(x),Constant(5))")
  }

  "An addition expression" should
     "spit its abstract syntax" in {
    val anAddition = records.ParseLib.parsestatement("3+4")
    anAddition.toString() should be ("Plus(Constant(3),Constant(4))")
  }

  "A compound multiplication and addition" should
     "spit out its abstract syntax" in {
    val aCompound = records.ParseLib.parsestatement("3*4 + 4")
    aCompound.toString() should be ("Plus(Times(Constant(3),Constant(4)),Constant(4))")
  }

  "A while statement with assignment" should
     "spit out its abstract syntax" in {
    val aWhile = records.ParseLib.parsestatement("while (5) x = 2;")
    aWhile.toString() should be ("While(Constant(5),Assignment(Variable(x),Constant(2)))")
  }
}
