package com.my.thing

import org.scalatest._

class ParserSpec extends FlatSpec {

	"The parser" should "do stuff" in {

		val parsedExpression = Parser.parse("and(equals('user.name', true),equals('user.name', true))").get

		implicit val a = A("a", Seq(1), B("1"))

		println(parsedExpression(a))



	}

	

}



