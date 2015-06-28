package com.my.thing

import org.scalatest._

class EvaluatorSpec extends FlatSpec {

	"The evaluator" should "do stuff" in {



		//println(Evaluator.R(Evaluator.S(Seq(1)),"2").asMap)

		val a = A("a", Seq(1), B("1"))

	}

}

// def getMethods[T: TypeTag] = typeOf[T].declarations.collect {
//   case m: MethodSymbol if m.isCaseAccessor => m
// }.toList

// for {field <- getMethods[A]} {
//     m.reflect(A).reflectField(field).get
//   }



