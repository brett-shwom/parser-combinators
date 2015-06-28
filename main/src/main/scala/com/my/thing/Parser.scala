package com.my.thing

import scala.util.parsing.combinator._

object Parser extends RegexParsers {
  def always : Parser[String]             = "always()"
  def equals: Parser[String]              = """equals(""" ~ fieldKeypath  ~ "," ~ ( booleanLiteral | stringLiteral | numericLiteral ) ~ ")" ^^ { _.toString }
  def greaterThan: Parser[String]         = """greaterThan("""       ^^ { _.toString }
  def greaterThanOrEqual: Parser[String]  = """greaterThanOrEqual("""     ^^ { _.toString }
  def lessThan: Parser[String]            = """lessThan("""     ^^ { _.toString }
  def lessThanOrEqual: Parser[String]     = """lessThanOrEqual("""       ^^ { _.toString }
  def contains: Parser[String]            = """contains("""       ^^ { _.toString }
  def and: Parser[String]                 = """and(""" ~ (comparisonExpression | operatorExpression) ~ "," ~ (comparisonExpression | operatorExpression) ~ ")" ^^ { _.toString }
  def or: Parser[String]                  = """or(""" ^^ { _.toString }
  def not: Parser[String]                 = """not(""" ^^ { _.toString }
  def fieldKeypath : Parser[String]       = """'[a-z\.]+'""".r ^^ { _.toString }
  def numericLiteral : Parser[String]     = """[0-9]+""".r
  def stringLiteral : Parser[String]      = """'[^\']+'""".r | """\"[^\"]+\"""".r //" trick to get sublime text highlighting to work properly
  def booleanLiteral : Parser[String]     = "true" | "false"
  def comparisonExpression : Parser[String]   = (
    equals | 
    greaterThan | 
    greaterThanOrEqual |
    lessThan |
    lessThanOrEqual |
    contains
    )
  def operatorExpression : Parser[String] = and | or | not
  def expression = operatorExpression | comparisonExpression | always

  def parse(stringToParse: String)        = parseAll(expression, stringToParse)

  // def number: Parser[Int]    = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  // def freq: Parser[WordFreq] = word ~ number        ^^ { case wd ~ fr => WordFreq(wd,fr) }
} 

