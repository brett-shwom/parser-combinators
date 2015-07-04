package com.my.thing

import org.scalatest._

class CaseClassToKeypathMapMacroSpec extends WordSpec with Matchers {


	"The CaseClassToKeypathMap macro | non-nested functionality" when {

		"passed an instance of a case class with no properties" should  {
			"""generate an empty map""" in {
				case class A()

				val a = A()

				CaseClassToKeypathMapMacro[A](a) should equal (Map())

			}
		}		

		"passed an instance of a case class with a String property" should  {
			"""generate a map like ("aString" -> a.aString)""" in {
				case class A(aString : String)

				val a = A("something")

				CaseClassToKeypathMapMacro[A](a) should equal (Map("aString" -> a.aString))

			}
		}

		"passed an instance of a case class with an Int property" should  {
			"""generate a map like ("anInt" -> a.anInt)""" in {
				case class A(anInt : Int)

				val a = A(1)

				CaseClassToKeypathMapMacro[A](a) should equal (Map("anInt" -> a.anInt))

			}
		}

		"passed an instance of a case class with more than one property" should  {
			"""generate a map like ("property1" -> a.property1, "property2" -> a.property2)""" in {
				case class A(anInt : Int, aString : String)

				val a = A(1, "1")

				CaseClassToKeypathMapMacro[A](a) should equal (Map("anInt" -> a.anInt, "aString" -> a.aString))

			}
		}

		"passed an instance of a case class with an Seq[Int] property" should  {
			"""generate a map like ("aSeqInt" -> a.aSeqInt)""" in {
				case class A(aSeqInt : Seq[Int])

				val a = A(Seq(1))

				CaseClassToKeypathMapMacro[A](a) should equal (Map("aSeqInt" -> a.aSeqInt))

			}
		}
	}

	"The CaseClassToKeypathMap macro | nested functionality" when {

		"passed an instance of a case class with an Seq[SomeOtherCaseClass] property" should  {
			"""generate a map like ("aSeqOfCaseClassB" -> a.aSeqOfCaseClassB)""" in {
				case class A(aSeqOfCaseClassB : Seq[B])
				case class B(anInt : Int)

				val a = A(Seq(B(1)))

				CaseClassToKeypathMapMacro[A](a) should equal (Map("aSeqOfCaseClassB" -> a.aSeqOfCaseClassB))

			}
		}

		"passed an instance of a case class with a SomeOtherCaseClass property" should  {
			"""generate a map like ("anInstanceOfSomeOtherCaseClass.anInt" -> a.anInstanceOfSomeOtherCaseClass.anInt)""" in {
				case class A(anInstanceOfSomeOtherCaseClass : B)
				case class B(anInt : Int)

				val a = A(B(1))

				CaseClassToKeypathMapMacro[A](a) should equal (Map("anInstanceOfSomeOtherCaseClass.anInt" -> a.anInstanceOfSomeOtherCaseClass.anInt))

			}
		}

		"passed an instance of a case class with a SomeOtherCaseClass property and that case class has more than one property" should  {
			"""generate a map like ("anInstanceOfSomeOtherCaseClass.property1" -> a.anInstanceOfSomeOtherCaseClass.property1,"anInstanceOfSomeOtherCaseClass.property2" -> a.anInstanceOfSomeOtherCaseClass.property2)""" in {
				case class A(anInstanceOfSomeOtherCaseClass : B)
				case class B(anInt : Int, aString : String)

				val a = A(B(1, "1"))

				CaseClassToKeypathMapMacro[A](a) should equal (Map(
					"anInstanceOfSomeOtherCaseClass.anInt" -> a.anInstanceOfSomeOtherCaseClass.anInt,
					"anInstanceOfSomeOtherCaseClass.aString" -> a.anInstanceOfSomeOtherCaseClass.aString
				))

			}
		}


		"passed an instance of a case class with an Option[Int] property" should  {

			case class A(anOptionInt : Option[Int])

			"""generate a map when that Option is Some[Int] like ("anOptionInt" -> a.anOptionInt)""" in {
				
				val a = A(Some(1))

				CaseClassToKeypathMapMacro[A](a) should equal (Map("anOptionInt" -> a.anOptionInt))

			}
			"""generate a map when that Option is None like ("anOptionInt" -> None)""" in {

				val a = A(None)

				CaseClassToKeypathMapMacro[A](a) should equal (Map("anOptionInt" -> None))

			}
		}

		"passed an instance of a case class with an Option[SomeOtherCaseClass] property" should  {

			case class A(anOptionOfCaseClassB : Option[B])
			case class B(anInt : Int)

			"""generate a map when that Option is Some[SomeOtherCaseClass] like ("anOptionOfCaseClassB.anInt" -> a.anOptionOfCaseClassB.map(anInt -> anInt)""" in {

				val a = A(Some(B(1)))

				CaseClassToKeypathMapMacro[A](a) should equal (Map("anOptionOfCaseClassB.anInt" -> Some(a.anOptionOfCaseClassB.get.anInt)))

			}
			"""generate a map when that Option is None like ("anOptionOfCaseClassB.anInt" -> None)""" in {

				val a = A(None)

				CaseClassToKeypathMapMacro[A](a) should equal (Map("anOptionOfCaseClassB.anInt" -> None))

			}
		}


	}

	"The CaseClassToKeypathMap macro | nested functionality | Option flattening" when {

		"passed an instance of a case class with an Option[SomeOtherCaseClassWhichItselfHasAnOption[Int]] property" should  {
			//i.e. the options in the values should be flattened to one single option


			case class A(anOptionOfCaseClassB : Option[B])
			case class B(anOptionInt : Option[Int])

			"""generate a map when that Option is Some[SomeOtherCaseClassWhichItselfHasAnOption[Int]] like ("anOptionOfSomeOtherCaseClassWhichItselfHasAnOption.anOptionInt" -> a.anOptionOfSomeOtherCaseClassWhichItselfHasAnOption.flatMap(anOptionInt -> anOptionInt)""" in {

				val a = A(Some(B(Some(1))))

				CaseClassToKeypathMapMacro[A](a) should equal (Map("anOptionOfCaseClassB.anOptionInt" -> Some(a.anOptionOfCaseClassB.get.anOptionInt.get)))

			}
			"""generate a map when that Option is Some(SomeOtherCaseClassWhichItselfHasAnOption(None)) like ("anOptionOfSomeOtherCaseClassWhichItselfHasAnOption.anOptionInt" -> None)""" in {

				val a = A(Some(B(None)))

				CaseClassToKeypathMapMacro[A](a) should equal (Map("anOptionOfCaseClassB.anInt" -> None))

			}
		}

	}	

}

