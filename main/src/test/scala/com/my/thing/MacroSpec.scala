package com.my.thing

import org.scalatest._

import EvaluatorWrapperClasses._

class MacroSpec extends FlatSpec with Matchers {

	"The KeypathMap macro" should "generate the correct map" in {



		val s = Simple(Some("f1.5"))

		val x = Macro[Simple](s)

		println(x)

		val l = LessSimple(Some("f1.5"), 2)

		val y = Macro[LessSimple](l)

		println("y" + y)


		val a = A("f1", Seq(1), B("bf1"))

		val z = Macro[A](a)

		println("z" + z)

		val aa = A("f1", Seq(1), B("bf1", Some(C("1"))))

		val zz = Macro[A](aa)

		println("zz" + zz)

	}

	

}

