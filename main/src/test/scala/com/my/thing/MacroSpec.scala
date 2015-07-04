package com.my.thing

import org.scalatest._

import EvaluatorWrapperClasses._

class MacroSpec extends WordSpec with Matchers {


	"The KeypathMap macro" when {
		"passed an instance of a case class with a String property" should  {
			"generate a proper map" in {
				case class A(aString : String)

				val a = A("something")

				Macro[A](a) should equal (Map("aString" -> a.aString))

			}
		}

		"passed an instance of a case class with an Int property" should  {
			"generate a proper map" in {
				case class A(anInt : Int)

				val a = A(1)

				Macro[A](a) should equal (Map("anInt" -> a.anInt))

			}
		}

		"passed an instance of a case class with an Seq[Int] property" should  {
			"generate a proper map" in {
				case class A(aSeqInt : Seq[Int])

				val a = A(Seq(1))

				Macro[A](a) should equal (Map("aSeqInt" -> a.aSeqInt))

			}
		}

		"passed an instance of a case class with an Seq[SomeOtherCaseClass] property" should  {
			"generate a proper map" in {
				case class A(aSeqOfCaseClassB : Seq[B])
				case class B(anInt : Int)

				val a = A(Seq(B(1)))

				Macro[A](a) should equal (Map("aSeqOfCaseClassB" -> a.aSeqOfCaseClassB))

			}
		}

		"passed an instance of a case class with a SomeOtherCaseClass property" should  {
			"generate a proper map" in {
				case class A(anInstanceOfSomeOtherCaseClass : B)
				case class B(anInt : Int)

				val a = A(B(1))

				Macro[A](a) should equal (Map("anInstanceOfSomeOtherCaseClass.anInt" -> a.anInstanceOfSomeOtherCaseClass.anInt))

			}
		}

		"passed an instance of a case class with an Option[Int] property" should  {

			case class A(anOptionInt : Option[Int])

			"generate a proper map when that Option is Some[Int]" in {
				
				val a = A(Some(1))

				Macro[A](a) should equal (Map("anOptionInt" -> Some(a.anOptionInt)))

			}
			"generate a proper map when that Option is None" in {

				val a = A(None)

				Macro[A](a) should equal (Map("anOptionInt" -> None))

			}
		}

		"passed an instance of a case class with an Option[SomeOtherCaseClass] property" should  {

			case class A(anOptionOfCaseClassB : Option[B])
			case class B(anInt : Int)

			"generate a proper map when that Option is Some[SomeOtherCaseClass]" in {

				val a = A(Some(B(1)))

				Macro[A](a) should equal (Map("anOptionOfCaseClassB.anInt" -> Some(a.anOptionOfCaseClassB.get.anInt)))

			}
			"generate a proper map when that Option is None" in {

				val a = A(None)

				Macro[A](a) should equal (Map("anOptionOfCaseClassB.anInt" -> None))

			}
		}

	}	

}

