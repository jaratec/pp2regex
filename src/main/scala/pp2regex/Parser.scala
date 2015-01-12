package pp2regex

import scala.util.parsing.combinator._

class Parser extends RegexParsers {

  // literals (letters, digits, space, slash and signs)
  def lit: Parser[Literal] = "[a-zA-Z0-9 \\-+/]+".r ^^ {s => Literal(s)}

  def digits: Parser[Digits] = "#+".r ^^ {xs => Digits(xs.size)}

  def letters: Parser[Letters] = "\\?+".r ^^ {xs => Letters(xs.size)}

  def uppercaseLetters: Parser[UppercaseLetters] = "&+".r ^^ {xs => UppercaseLetters(xs.size)}

  def ascii: Parser[Ascii] = "@+".r ^^ {xs => Ascii(xs.size)}

  def uppercaseAscii: Parser[UppercaseAscii] = "!+".r ^^ {xs => UppercaseAscii(xs.size)}

  def simpleExpr: Parser[SimpleExpr] = uppercaseAscii | ascii | uppercaseLetters | letters | digits | lit

  def optional: Parser[Optional] = "[" ~> simpleExpr <~ "]" ^^ {e => Optional(e)}

  def alternative: Parser[Alternative] =  "{" ~> repsep(simpleExpr, ",") <~ "}" ^^ {xs => Alternative(xs)}

  def repeatedExpr: Parser[RepeatedExpr] = "*" ~> "[0-9]+".r ~ expr ^^ {case n ~ e => RepeatedExpr(n.toInt,e)}

  def composedExpr: Parser[ComposedExpr] = alternative | optional | repeatedExpr

  def expr: Parser[Expr] = simpleExpr | composedExpr

  def picture: Parser[List[Expr]] = rep(expr)

  override def skipWhitespace:Boolean = false

  def parse(str: String): Option[String] = {
    parseAll(picture, str) match {
      case Failure(msg, next) => println("Could not parse string " + str + " - " + msg); None
      case Error(msg, next) => println("Could not parse string " + str + " - " + msg); None
      case Success(result, next) => Some(result.mkString)
    }
  }

}
