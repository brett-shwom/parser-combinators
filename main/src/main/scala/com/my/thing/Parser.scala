package com.my.thing

import scala.util.parsing.combinator._

import EvaluatorWrapperClasses._


object Parser extends RegexParsers {
  type ContextualBooleanFunction  = (A) => Boolean
  //type BooleanFunction = () => Boolean

  def always : Parser[ContextualBooleanFunction]    = "always()" ^^ { x => anything => true }
  def equals : Parser[ContextualBooleanFunction]     = """equals(""" ~ keypath  ~ "," ~ ( booleanLiteral | stringLiteral | longLiteral ) ~ ")" ^^ { x => 
      x match {
        case _ ~ keypath ~ _ ~ literal  ~ _ => {
          val a : (A) => Boolean = implicit a => KeypathLookup.lookup(keypath, a) equals literal
          a
        }
      }
  }
  // def greaterThan: Parser[ComparisonBooleanFunction]         = """greaterThan("""       ~ keypath  ~ "," ~ longLiteral ~ ")" ^^ { _.toString }
  // def greaterThanOrEqual: Parser[ComparisonBooleanFunction]  = """greaterThanOrEqual("""     ~ keypath  ~ "," ~ longLiteral ~ ")" ^^ { _.toString }
  // def lessThan: Parser[ComparisonBooleanFunction]            = """lessThan("""     ~ keypath  ~ "," ~ longLiteral ~ ")" ^^ { _.toString }
  // def lessThanOrEqual: Parser[ComparisonBooleanFunction]     = """lessThanOrEqual("""       ~ keypath  ~ "," ~ longLiteral ~ ")" ^^ { _.toString }
  // def contains: Parser[ComparisonBooleanFunction]            = """contains("""       ~ keypath  ~ "," ~ ( booleanLiteral | stringLiteral | longLiteral ) ~ ")" ^^ { _.toString }
  def and: Parser[ContextualBooleanFunction] = """and(""" ~ (comparisonExpression | operatorExpression) ~ "," ~ (comparisonExpression | operatorExpression) ~ ")" ^^ { 
    case _ ~ booleanFunctionLeft ~ _ ~ booleanFunctionRight ~ _ => {
      println(booleanFunctionLeft)
      println(booleanFunctionRight)

      // booleanFunctionLeft && booleanFunctionRight

      (x) => true
    }
  }
  // def or: Parser[String]                  = """or(""" ^^ { _.toString }
  // def not: Parser[String]                 = """not(""" ^^ { _.toString }
  def keypath : Parser[String]            = """'[a-z\.]+'""".r ^^ { _.toString }
  def longLiteral : Parser[Long]          = """[0-9]+""".r ^^ { _.toLong }
  def stringLiteral : Parser[String]      = """'[^\']+'""".r | """\"[^\"]+\"""".r //" trick to get sublime text highlighting to work properly
  def booleanLiteral : Parser[Boolean]    = ("true" | "false") ^^ { trueOrFalseString => if (trueOrFalseString == "true") true else false }
  
  def comparisonExpression : Parser[ContextualBooleanFunction]   = (
    equals //| 
    // greaterThan | 
    // greaterThanOrEqual |
    // lessThan |
    // lessThanOrEqual |
    // contains
    )
  def operatorExpression : Parser[ContextualBooleanFunction] = and //| or | not
  def expression = operatorExpression | comparisonExpression | always

  def parse(stringToParse: String)        = parseAll(expression, stringToParse)

  // def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  // def freq: Parser[WordFreq] = word ~ number        ^^ { case wd ~ fr => WordFreq(wd,fr) }
} 

