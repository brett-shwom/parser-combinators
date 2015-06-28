package com.my.thing

import org.scalatest._

import EvaluatorWrapperClasses._

class EvaluatorWrapperClassesSpec extends FlatSpec with Matchers{

	"The evaluator wrapper classes" should "make the proper equals comparisons" in {

		ComparableOptionLong(Some(22)) should equal(ComparableOptionLong(Some(22)))
		// ComparableOptionLong(Some(22)) should equal(ComparableOptionInt(Some(22)))
		// ComparableOptionInt(Some(22)) should equal(ComparableOptionLong(Some(22)))

		ComparableOptionString(Some("a")) should equal(ComparableOptionString(Some("a")))
		ComparableOptionBoolean(Some(true)) should equal(ComparableOptionBoolean(Some(true)))

		//ComparableSeqInt(Seq(1)) should equal(ComparableSeqInt(Seq(1)))
		ComparableSeqLong(Seq(1L)) should equal(ComparableSeqLong(Seq(1L)))
		ComparableSeqLong(Seq(1)) should equal(ComparableSeqLong(Seq(1L)))
		ComparableSeqLong(Seq(1L)) should equal(ComparableSeqLong(Seq(1)))
		ComparableSeqLong(Seq(1)) should equal(ComparableSeqLong(Seq(1)))

		// ComparableSeqInt(Seq(1)) should equal(ComparableSeqLong(Seq(1L)))
		// ComparableSeqInt(Seq(1)) should equal(ComparableSeqLong(Seq(1)))

		//ComparableSeqLong(Seq(1L)) should equal(ComparableSeqInt(Seq(1)))
		//ComparableSeqLong(Seq(1)) should equal(ComparableSeqInt(Seq(1)))


	}

	

}