package records

object Validator {

  def validate(args: Statement): Boolean = args match {
    case Constant(_) =>
      true
    case Plus(a, b) =>
      (validate(a) && validate(b))
    case Minus(a,b) =>
      (validate(a) && validate(b))
    case Times(a,b) =>
      (validate(a) && validate(b))
    case Div(a,b) =>
      (validate(a) && validate(b))
    case Variable(_) =>
      true
    case Sequence(multistatements @ _*) => {
      val stmts = multistatements.toList.map(validate(_)) //map statements to their validity
      val result = stmts.foldLeft(true)((a:Boolean, b:Boolean) => {
        (a && b)
      })
      result
    }
    case While(guard, body) =>
      guard match {
        case Constant(_) =>
          true
        case Plus(_,_) =>
          true
        case Minus(_,_) =>
          true
        case Times(_,_) =>
          true
        case Div(_,_) =>
          true
        case Variable(_) =>
          true
        case Sequence(stmts @ _*) =>
          validate(stmts.toList.last)
        case While(_,_) =>
          false
        case Assignment(left,right) =>
          left match {
            case Variable(_) =>
              true
            case Selection(receiver,field) =>
              true
            case (_) =>
              false
          }                    
        case New(_) =>
          false
      }
    case Assignment(left,right) =>
      left match {
        case Variable(_) =>
          true
        case Selection(receiver,field) =>
          true
        case (_) =>
          false
      }
    case New(_) =>
      true
    case Selection(receiver,value) =>
      true
  }
}
