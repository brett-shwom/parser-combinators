package com.my.thing

import scala.util.parsing.combinator._

import EvaluatorWrapperClasses._


object Parser extends RegexParsers {
  type ContextualBooleanFunction  = (A) => Boolean

  def always : Parser[ContextualBooleanFunction]    = "always()" ^^ { x => anything => true }
  def equals : Parser[ContextualBooleanFunction]     = """equals(""" ~ keypath  ~ "," ~ ( booleanLiteral | stringLiteral | longLiteral ) ~ ")" ^^ { x => 
      x match {
        case _ ~ keypath ~ _ ~ literal  ~ _ => {
          a => {
            KeypathLookup.lookup(keypath, a) equals literal
          }
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
      (a) => booleanFunctionLeft(a) && booleanFunctionRight(a)
    }
  }

  def or: Parser[ContextualBooleanFunction] = """or(""" ~ (comparisonExpression | operatorExpression) ~ "," ~ (comparisonExpression | operatorExpression) ~ ")" ^^ { 
    case _ ~ booleanFunctionLeft ~ _ ~ booleanFunctionRight ~ _ => {
      (a) => booleanFunctionLeft(a) || booleanFunctionRight(a)
    }
  }
  
  def not: Parser[ContextualBooleanFunction] = """not(""" ~ (comparisonExpression | operatorExpression) ~ ")" ^^ { 
    case _ ~ booleanFunction ~ _ => {
      (a) => !booleanFunction(a)
    }
  }

  def keypath : Parser[String]            = (("'" ~ """[_0-9A-Za-z\.]+""".r ~ "'") | ( "\"" ~ """[_0-9A-Za-z\.]+""".r  ~ "\"" )) ^^ { 
    case _ ~ keypath ~  _ => keypath
  }

  def longLiteral : Parser[Long]          = """[0-9]+""".r ^^ { _.toLong }

  def stringLiteral : Parser[String]      = (("'" ~ """[^\']+""".r ~ "'") | ( "\"" ~ """[^\"]+""".r  ~ "\"" )) ^^ { 
    case _ ~ stringLiteral ~  _ => stringLiteral
  } 
  def booleanLiteral : Parser[Boolean]    = ("true" | "false") ^^ { trueOrFalseString => if (trueOrFalseString == "true") true else false }
  
  def comparisonExpression : Parser[ContextualBooleanFunction]   = (
    equals //| 
    // greaterThan | 
    // greaterThanOrEqual |
    // lessThan |
    // lessThanOrEqual |
    // contains
    )
  def operatorExpression : Parser[ContextualBooleanFunction] = and | or | not
  def expressionRoot = operatorExpression | comparisonExpression | always

  def parse(stringToParse: String)        = parseAll(expressionRoot, stringToParse)

} 

