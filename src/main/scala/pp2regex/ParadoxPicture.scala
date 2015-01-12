package pp2regex

trait Expr {}

trait SimpleExpr extends Expr {}

case class Literal(val s: String) extends SimpleExpr {
  override def toString(): String = s
}

case class Digits(val n: Int) extends SimpleExpr {
  override def toString(): String = if (n > 1) "[0-9]{" + n + "}" else "[0-9]"
}

case class Letters(val n: Int) extends SimpleExpr {
  override def toString(): String = if (n > 1) "[a-zA-Z]{" + n + "}" else "[a-zA-Z]"
}

case class UppercaseLetters(val n: Int) extends SimpleExpr {
  override def toString(): String = if (n > 1) "[A-Z]{" + n + "}" else "[A-Z]"
}

case class Ascii(val n: Int) extends SimpleExpr {
  override def toString(): String = if (n > 1) "\\p{ASCII}{" + n + "}" else "\\p{ASCII}"
}

case class UppercaseAscii(val n: Int) extends SimpleExpr {
  override def toString(): String = if (n > 1) "[\\p{ASCII}&&[^a-z]]{" + n + "}" else "[\\p{ASCII}&&[^a-z]]"
}

trait ComposedExpr extends Expr {}

case class RepeatedExpr(val n: Int, val e: Expr) extends ComposedExpr {
  override def toString(): String = e match {
    case x: Alternative => x + "{" + n + "}"
    case _ => "(" + e + "){" + n + "}"
  }
}

case class Optional(val e: Expr) extends ComposedExpr {
  override def toString(): String = e match {
    case x: Literal => "(" + x + ")?"
    case _ => e + "?"
  }
}

case class Alternative(val es: List[Expr]) extends ComposedExpr {
  override def toString(): String = "(" + es.mkString("|") + ")"
}
