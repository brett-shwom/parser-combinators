package com.my.thing

import EvaluatorWrapperClasses._
import scala.util.parsing.combinator._
import scala.util.Try


trait Parser  {
  type ContextualBooleanFunction  = (A) => Boolean

  def parse(stringToParse: String) : Try[ContextualBooleanFunction]

}

object Parser extends RegexParsers with Parser {


  def always : Parser[ContextualBooleanFunction] = "always()" ^^ { x => anything => true }

  def equals : Parser[ContextualBooleanFunction] = """equals(""" ~ keypath  ~ "," ~ ( booleanLiteral | stringLiteral | longLiteral ) ~ ")" ^^ {  
    _ match {
      case _ ~ keypath ~ _ ~ literal  ~ _ => {
        a => KeypathLookup.lookup(keypath, a) equals literal
      }
    }
      
  }
  def greaterThan: Parser[ContextualBooleanFunction] = """greaterThan("""  ~ keypath  ~ "," ~ longLiteral ~ ")" ^^ { 
    _ match {
      case _ ~ keypath ~ _ ~ literal  ~ _ => {
        a => {
          KeypathLookup.lookup(keypath, a) greaterThan literal
        }
      }
    }
  }

  def greaterThanOrEqual: Parser[ContextualBooleanFunction] = """greaterThanOrEqual(""" ~ keypath  ~ "," ~ longLiteral ~ ")" ^^ { 
    _ match {
      case _ ~ keypath ~ _ ~ literal  ~ _ => {
        a => {
          KeypathLookup.lookup(keypath, a) greaterThanOrEqual literal
        }
      }
    }
  }

  def lessThan: Parser[ContextualBooleanFunction] = """lessThan(""" ~ keypath  ~ "," ~ longLiteral ~ ")" ^^ { 
    _ match {
      case _ ~ keypath ~ _ ~ literal  ~ _ => {
        a => {
          KeypathLookup.lookup(keypath, a) lessThan literal
        }
      }
    }
  }

  def lessThanOrEqual: Parser[ContextualBooleanFunction]         = """lessThanOrEqual(""" ~ keypath  ~ "," ~ longLiteral ~ ")" ^^ { 
    _ match {
      case _ ~ keypath ~ _ ~ literal  ~ _ => {
        a => {
          KeypathLookup.lookup(keypath, a) lessThanOrEqual literal
        }
      }
    }
  }

  def contains: Parser[ContextualBooleanFunction]         = """contains(""" ~ keypath  ~ "," ~ longLiteral ~ ")" ^^ { 
    _ match {
      case _ ~ keypath ~ _ ~ literal  ~ _ => {
        a => {
          KeypathLookup.lookup(keypath, a) contains literal
        }
      }
    }
  }

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

  def keypath : Parser[String]   = (("'" ~ """[_0-9A-Za-z\.]+""".r ~ "'") | ( "\"" ~ """[_0-9A-Za-z\.]+""".r  ~ "\"" )) ^^ { 
    case _ ~ keypath ~  _ => keypath
  }

  //look ma, no Ints!
  def longLiteral : Parser[ComparableOptionLong]  = """[0-9]+""".r ^^ { l => ComparableOptionLong(Some(l.toLong)) }

  //TODO: BigDecimals?

  //TODO: support escaped quotes...use something from JavaTokenParsers
  def stringLiteral : Parser[ComparableOptionString] = (("'" ~ """[^\']+""".r ~ "'") | ( "\"" ~ """[^\"]+""".r  ~ "\"" )) ^^ { 
    case _ ~ stringLiteral ~  _ => ComparableOptionString(Some(stringLiteral))
  } 
  def booleanLiteral : Parser[ComparableOptionBoolean] = ("true" | "false") ^^ { trueOrFalseString => if (trueOrFalseString == "true") ComparableOptionBoolean(Some(true)) else ComparableOptionBoolean(Some(false)) }
  
  def comparisonExpression : Parser[ContextualBooleanFunction] = (
    equals | 
    greaterThan | 
    greaterThanOrEqual |
    lessThan |
    lessThanOrEqual |
    contains
    )

  def operatorExpression : Parser[ContextualBooleanFunction] = and | or | not

  def expressionRoot = operatorExpression | comparisonExpression | always

  def parse(stringToParse: String) = parseAll(expressionRoot, stringToParse) match {
    case Success(result, next) => scala.util.Success(result)
    case NoSuccess(message, next) => scala.util.Failure(new Exception(message)) //new Exception, gross...
  }

} 

