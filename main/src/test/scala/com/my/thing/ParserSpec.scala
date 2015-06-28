package com.my.thing

import org.scalatest._

class ParserSpec extends FlatSpec {

	"The parser" should "do stuff" in {

		println(Parser.parse("and(equals('user.name', true),equals('user.name', true)) "))

	}

	

}



