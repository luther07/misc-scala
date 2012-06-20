package records

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class BDDTests extends FlatSpec with ShouldMatchers {

  "A constant" should
     "spit out its abstract syntax" in {
    val aConst = records.ParseLib.parsestatement("1")
    aConst should be (Constant(1))
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
     "spit out its abstract syntax" in {
    val anAddition = records.ParseLib.parsestatement("3+4")
    anAddition.toString() should be ("Plus(Constant(3),Constant(4))")
  }

  "A subtraction expression" should
     "spit out its abstract syntax" in {
    val aSubtraction = records.ParseLib.parsestatement("3-4")
    aSubtraction.toString() should be ("Minus(Constant(3),Constant(4))")
  }

  "A multiplication expression" should
     "spit out its abstract syntax" in {
    val aMultiplication = records.ParseLib.parsestatement("3*4")
    aMultiplication.toString() should be ("Times(Constant(3),Constant(4))")
  }

  "A division expression" should
     "spit out its abstract syntax" in {
    val aDivision = records.ParseLib.parsestatement("3/4")
    aDivision.toString() should be ("Div(Constant(3),Constant(4))")
  }

  "A compound expression" should
     "spit out its abstract syntax" in {
    val thisCompound = records.ParseLib.parsestatement("3/4+4*3")
    thisCompound.toString() should be ("Plus(Div(Constant(3),Constant(4)),Times(Constant(4),Constant(3)))")
  }

  "A compound multiplication and addition" should
     "spit out its abstract syntax" in {
    val aCompound = records.ParseLib.parsestatement("3*4 + 4")
    aCompound.toString() should be ("Plus(Times(Constant(3),Constant(4)),Constant(4))")
  }

  "A while statement with assignment" should
     "spit out its abstract syntax" in {
    val aWhile = records.ParseLib.parsestatement("while (5) { x = 2; };")
    aWhile.toString() should be ("While(Constant(5),Assignment(Variable(x),Constant(2)))")
  }

  "An assignment of variable to a new" should
     "spit out its abstract syntax" in {
    val aNew = records.ParseLib.parsestatement("x = new y;")
    aNew.toString() should be ("Assignment(Variable(x),New(Clazz(WrappedArray(y))))")
  }

  "An assignment of a struct field" should
     "spit out its abstract syntax" in {
    val aStructField = records.ParseLib.parsestatement("r.course1 = new StudentCourseRecord;")
    aStructField.toString() should be ("Assignment(Selection(Variable(r),course1),New(Clazz(WrappedArray(StudentCourseRecord))))")
  }


}
