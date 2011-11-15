package records

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Assert._


@RunWith(classOf[JUnitRunner])
class TestRecords extends FunSuite{
  
  
  test("Assignment to ListNode") {
    val listNode = Clazz("value", "next")
    var store = Map[String, records.Cell](
      "n" -> Cell(0),
      "h" -> Cell(0),
      "s" -> Cell(0)
    )
    
    val s = Assignment(Variable("n"), New(listNode))
    assertEquals(s.toString(), "Assignment(Variable(n),New(Clazz(WrappedArray(value, next))))")
    Execute(store)(s)
    assertEquals(store.toString(), "Map(n -> Cell(Right(Map(value -> Cell(Left(0)), next -> Cell(Left(0))))), h -> Cell(Left(0)), s -> Cell(Left(0)))")
  }
  
  test("Assignment to Variable"){
    var store = Map[String, records.Cell](
      "n" -> Cell(0),
      "h" -> Cell(0),
      "s" -> Cell(0)
    )
    
    val s = Assignment(Variable("h"), Variable("n"))
    assertEquals(s.toString(), "Assignment(Variable(h),Variable(n))")
    Execute(store)(s)
    assertEquals(store.toString(), "Map(n -> Cell(Left(0)), h -> Cell(Left(0)), s -> Cell(Left(0)))")
  }
  
  test("Assignment to Selection"){
    val listNode = Clazz("value", "next")
    var store = Map[String, records.Cell](
      "n" -> Cell(0),
      "h" -> Cell(0),
      "s" -> Cell(0)
    )
    
    val s = Sequence(Assignment(Variable("n"), New(listNode)),
        Assignment(Selection(Variable("n"), "value"), Constant(2)))
    assertEquals(s.toString(), "Sequence(WrappedArray(Assignment(Variable(n),New(Clazz(WrappedArray(value, next)))), Assignment(Selection(Variable(n),value),Constant(2))))")
    Execute(store)(s)
    assertEquals(store.toString, "Map(n -> Cell(Right(Map(value -> Cell(Left(2)), next -> Cell(Left(0))))), h -> Cell(Left(0)), s -> Cell(Left(0)))")
  
  }
  
  test("While"){
    val listNode = Clazz("value", "next")
    var store = Map[String, records.Cell](
      "n" -> Cell(0),
      "h" -> Cell(0),
      "s" -> Cell(0)
    )
    
    val s = Sequence(Assignment(Variable("n"), New(listNode)),
        Sequence(
          Assignment(Variable("s"), Plus(Variable("s"), Selection(Variable("n"), "value"))),
          Assignment(Variable("n"), Selection(Variable("n"), "next"))
        ))
    assertEquals(s.toString(), "Sequence(WrappedArray(Assignment(Variable(n),New(Clazz(WrappedArray(value, next)))), Sequence(WrappedArray(Assignment(Variable(s),Plus(Variable(s),Selection(Variable(n),value))), Assignment(Variable(n),Selection(Variable(n),next))))))")
    Execute(store)(s)
    assertEquals(store.toString() , "Map(n -> Cell(Left(0)), h -> Cell(Left(0)), s -> Cell(Left(0)))")
  }
  
  test("Sequence 1"){
    val listNode = Clazz("value", "next")
    var store = Map[String, records.Cell](
      "n" -> Cell(0),
      "h" -> Cell(0),
      "s" -> Cell(0)
    )
    
    val s = Sequence(
      Assignment(Variable("n"), New(listNode)),
      Assignment(Variable("h"), Variable("n")),
      Assignment(Selection(Variable("n"), "value"), Constant(2)),
      Assignment(Selection(Variable("n"), "next"), New(listNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(3)),
      Assignment(Selection(Variable("n"), "next"), New(listNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(5)),
      Assignment(Selection(Variable("n"), "next"), New(listNode)),
      Assignment(Variable("n"), Selection(Variable("n"), "next")),
      Assignment(Selection(Variable("n"), "value"), Constant(7)),
      Assignment(Selection(Variable("n"), "next"), Constant(0)),
      Assignment(Variable("n"), Variable("h")),
      While(Variable("n"),
        Sequence(
          Assignment(Variable("s"), Plus(Variable("s"), Selection(Variable("n"), "value"))),
          Assignment(Variable("n"), Selection(Variable("n"), "next"))
        )
      )
    )
    assertEquals(s.toString(), "Sequence(WrappedArray(Assignment(Variable(n),New(Clazz(WrappedArray(value, next))))," +
    		" Assignment(Variable(h),Variable(n)), Assignment(Selection(Variable(n),value),Constant(2)), " +
    		"Assignment(Selection(Variable(n),next),New(Clazz(WrappedArray(value, next)))), Assignment(Variable(n),Selection(Variable(n),next)), " +
    		"Assignment(Selection(Variable(n),value),Constant(3)), Assignment(Selection(Variable(n),next),New(Clazz(WrappedArray(value, next))))," +
    		" Assignment(Variable(n),Selection(Variable(n),next)), Assignment(Selection(Variable(n),value),Constant(5)), Assignment(Selection(Variable(n),next)," +
    		"New(Clazz(WrappedArray(value, next)))), Assignment(Variable(n),Selection(Variable(n),next)), Assignment(Selection(Variable(n),value),Constant(7))," +
    		" Assignment(Selection(Variable(n),next),Constant(0)), Assignment(Variable(n),Variable(h)), While(Variable(n)," +
    		"Sequence(WrappedArray(Assignment(Variable(s),Plus(Variable(s),Selection(Variable(n),value))), Assignment(Variable(n)," +
    		"Selection(Variable(n),next)))))))")
    Execute(store)(s)
    assertEquals(store.toString() , "Map(n -> Cell(Left(0)), h -> Cell(Right(Map(value -> Cell(Left(2)), next -> Cell(Right(Map(value -> Cell(Left(3)), next -> Cell(Right(Map(value -> Cell(Left(5)), next -> Cell(Right(Map(value -> Cell(Left(7)), next -> Cell(Left(0)))))))))))))), s -> Cell(Left(17)))")
  }
  
  test("Sequence 2"){
    val studentCourseRecord = Clazz("firstExamScore", "secondExamScore", "totalScore")
    val studentSemRecord = Clazz("course1", "course2")
  
    val store = Map[String, Cell](
      "q" -> Cell(0),
      "r" -> Cell(0)
    )

    val s =
        Sequence(
          Assignment(Variable("r"), New(studentSemRecord)),
          Assignment(Selection(Variable("r"), "course1"), New(studentCourseRecord)),
          Assignment(Selection(Selection(Variable("r"), "course1"), "firstExamScore"), Constant(25)),
          Assignment(Selection(Selection(Variable("r"), "course1"), "secondExamScore"), Constant(35)),
          Assignment(Selection(Selection(Variable("r"), "course1"), "totalScore"),
                             Plus(Selection(Selection(Variable("r"), "course1"), "firstExamScore"),
                                      Selection(Selection(Variable("r"), "course1"), "secondExamScore"))),
          Assignment(Selection(Variable("r"), "course2"), Selection(Variable("r"), "course1")),
          Assignment(Variable("q"), Selection(Selection(Variable("r"), "course2"), "totalScore")),
          Assignment(Selection(Selection(Variable("r"), "course1"), "firstExamScore"), Constant(45))
        )
    println(s)
    assertEquals(s.toString(), "Sequence(WrappedArray(Assignment(Variable(r),New(Clazz(WrappedArray(course1, course2)))), " +
    		"Assignment(Selection(Variable(r),course1),New(Clazz(WrappedArray(firstExamScore, secondExamScore, totalScore))))," +
    		" Assignment(Selection(Selection(Variable(r),course1),firstExamScore),Constant(25)), " +
    		"Assignment(Selection(Selection(Variable(r),course1),secondExamScore),Constant(35)), " +
    		"Assignment(Selection(Selection(Variable(r),course1),totalScore),Plus(Selection(Selection(Variable(r),course1),firstExamScore)," +
    		"Selection(Selection(Variable(r),course1),secondExamScore))), Assignment(Selection(Variable(r),course2),Selection(Variable(r),course1)), " +
    		"Assignment(Variable(q),Selection(Selection(Variable(r),course2),totalScore)), Assignment(Selection(Selection(Variable(r),course1),firstExamScore),Constant(45))))")
    Execute(store)(s)
    assertEquals(store.toString() , "Map(q -> Cell(Left(60)), r -> Cell(Right(Map(course1 -> Cell(Right(Map(firstExamScore -> Cell(Left(45)), " +
    		"secondExamScore -> Cell(Left(35)), totalScore -> Cell(Left(60))))), course2 -> Cell(Right(Map(firstExamScore -> Cell(Left(45)), " +
    		"secondExamScore -> Cell(Left(35)), totalScore -> Cell(Left(60)))))))))")
  
  }
  
  test("Online case 1"){
    var store = Map[String, Cell](
    )
    val s1 = "x = 5;";
    replIO.interpret(s1);
    val s2 = "var x;";
    replIO.interpret(s2);
    val s3 = "n = new ListNode;";
    replIO.interpret(s3);
  }
  
  
  test("Online case 2"){
    
  }
  
  test("Online case 3"){
    
  }
  
  test("Online case 4"){
    
  }



}

