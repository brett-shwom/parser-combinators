package com.my.thing

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {

	"The parser" should "handle equal strings" in {

		//val parsedExpression = Parser.parse("and(equals('user.name', true),equals('user.name', true))").get

		val a = A("str", Seq(1), B("1"))

		val evaluator1 = Parser.parse("equals('a.f1', 'str')").get

		evaluator1(a) should equal(true)

		val b = A("otherstr", Seq(1), B("1"))

		val evaluator2 = Parser.parse("equals('a.f1', 'str')").get

		evaluator2(b) should equal(false)


	}

	"The parser" should "handle equal Ints" in {

		//val parsedExpression = Parser.parse("and(equals('user.name', true),equals('user.name', true))").get

		val a = A("str", Seq(1), B("1"), 22)

		val evaluator1 = Parser.parse("equals('a.f4', 22)").get

		evaluator1(a) should equal(true)

		val b = A("str", Seq(1), B("1"), 22)

		val evaluator2 = Parser.parse("equals('a.f4', 4444)").get

		evaluator2(b) should equal(false)


	}

	"The parser" should "handle and()" in {

		val a = A("str", Seq(1), B("1"))

		val evaluator1 = Parser.parse("and(equals('a.f1', 'str'), equals('a.f1', 'str'))").get

		evaluator1(a) should equal(true)

		val evaluator2 = Parser.parse("and(equals('a.f1', 'str'), equals('a.f1', 'otherStr'))").get

		evaluator2(a) should equal(false)


	}

	"The parser" should "handle or()" in {

		val a = A("str", Seq(1), B("1"))

		val evaluator1 = Parser.parse("or(equals('a.f1', 'str'), equals('a.f1', 'str'))").get

		evaluator1(a) should equal(true)

		val evaluator2 = Parser.parse("or(equals('a.f1', 'otherStr'), equals('a.f1', 'str'))").get

		evaluator2(a) should equal(true)

		val evaluator3 = Parser.parse("or(equals('a.f1', 'otherStr'), equals('a.f1', 'otherStr2'))").get

		evaluator3(a) should equal(false)


	}

	"The parser" should "handle not()" in {

		val a = A("str", Seq(1), B("1"))

		val evaluator1 = Parser.parse("not(equals('a.f1', 'str'))").get

		evaluator1(a) should equal(false)

		val evaluator2 = Parser.parse("not(equals('a.f1', 'otherStr'))").get

		evaluator2(a) should equal(true)


	}


	

}



