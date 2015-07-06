package com.my.thing

import org.scalatest._

import EvaluatorWrapperClasses._

class EvaluatorWrapperClassesSpec extends FlatSpec with Matchers{

	"The evaluator wrapper classes" should "make the proper equals comparisons" in {

		ComparableOptionLong(Some(22)) should equal(ComparableOptionLong(Some(22)))

		ComparableOptionString(Some("a")) should equal(ComparableOptionString(Some("a")))
		ComparableOptionBoolean(Some(true)) should equal(ComparableOptionBoolean(Some(true)))

		ComparableSeqLong(Seq(1L)) should equal(ComparableSeqLong(Seq(1L)))
		ComparableSeqLong(Seq(1)) should equal(ComparableSeqLong(Seq(1L)))
		ComparableSeqLong(Seq(1L)) should equal(ComparableSeqLong(Seq(1)))
		ComparableSeqLong(Seq(1)) should equal(ComparableSeqLong(Seq(1)))




	}

	"The evaluator wrapper class converters" should "make the proper conversions" in {

		import EvaluatorWrapperConverters._

		1.asComparable should equal(ComparableOptionLong(Some(1)))
		1L.asComparable should equal(ComparableOptionLong(Some(1)))
		"a".asComparable should equal(ComparableOptionString(Some("a")))
		true.asComparable should equal(ComparableOptionBoolean(Some(true)))

		Seq(1).asComparable should equal(ComparableSeqLong(Seq(1L)))
		Seq(1L).asComparable should equal(ComparableSeqLong(Seq(1L)))
		Seq(true).asComparable should equal(ComparableSeqBoolean(Seq(true)))
		Seq("a").asComparable should equal(ComparableSeqString(Seq("a")))

	}

	

}