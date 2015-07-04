package com.my.thing

import org.scalatest._

import EvaluatorWrapperClasses._

class MacroSpec extends FlatSpec with Matchers {

	"The KeypathMap macro" should "generate the correct map" in {

		// val a = A("f1", Seq(1), B("bf1"))
		// case class A()
		// val b = A()

		val a = A("f1", Seq(1), B("bf1"))

		val x = Macro[A](a)

		println(x)

	}

	

}

