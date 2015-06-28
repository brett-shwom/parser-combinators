package com.my.thing

object EvaluatorWrapperClasses {

	/* Wrapper classes which :
		1) restrict the types that are allowed to be involved in a comparison
		2) allow for custom comparison behaviors when comparisons between different types occur
	*/

	sealed abstract class Comparable(value : Any) {
		def equals(other : Comparable) : Boolean = { //Maybe this should return a ComparisonResult of which there are 2 subtypes, ValidComparison and InvalidComparison

			(this, other) match {

				case (ComparableOptionString(Some(thisValue)), ComparableOptionString(Some(otherValue))) => thisValue equals otherValue 
				
				case (ComparableOptionLong(Some(thisValue)), ComparableOptionLong(Some(otherValue))) => thisValue equals otherValue 

				case (ComparableOptionBoolean(Some(thisValue)), ComparableOptionBoolean(Some(otherValue))) => thisValue equals otherValue


				case (ComparableSeqLong(thisValue), ComparableSeqLong(otherValue)) => thisValue equals otherValue

				case (ComparableSeqString(thisValue), ComparableSeqString(otherValue)) => thisValue equals otherValue

				case (ComparableSeqBoolean(thisValue), ComparableSeqBoolean(otherValue)) => thisValue equals otherValue

				case (_, _) => false //default rule, maybe we should distinguish between false and invalid comparisons?

				//TODO: should ComparableUndefined(_) equal ComparableUndefined(_) ?

			}

		}


	}


	// //TODO: should we have support for BigInt / float?

	//Option[T] only for primitives, makes it a bit easier to reason about the comparisons

	case class ComparableOptionString(value : Option[String]) extends Comparable(value)
	case class ComparableOptionLong(value : Option[Long]) extends Comparable(value)
	case class ComparableOptionBoolean(value : Option[Boolean]) extends Comparable(value)

	case class ComparableSeqLong(value : Seq[Long]) extends Comparable(value)
	case class ComparableSeqString(value : Seq[String]) extends Comparable(value)
	case class ComparableSeqBoolean(value : Seq[Boolean]) extends Comparable(value)

	case class ComparableUndefined() extends Comparable(None)

}

object EvaluatorWrapperConverters {

	import EvaluatorWrapperClasses._

	implicit def toComparableStringConverter(s:String) = new ComparableStringConverter(s)

	class ComparableStringConverter(s : String) {
		def asComparable = new ComparableOptionString(Some(s))
	}

	implicit def toComparableOptionStringConverter(s:Option[String]) = new ComparableOptionStringConverter(s)

	class ComparableOptionStringConverter(s : Option[String]) {
		def asComparable = new ComparableOptionString(s)
	}


	implicit def toComparableIntConverter(i: Int) = new ComparableIntConverter(i)

	class ComparableIntConverter(i : Int) {
		def asComparable = new ComparableOptionLong(Some(i.asInstanceOf[Long])) //convert the int to long - might be an expensive operation TODO: look into optimizing ex: lazy conversion etc.
	}

	implicit def toComparableOptionIntConverter(i:Option[Int]) = new ComparableOptionIntConverter(i)

	class ComparableOptionIntConverter(i : Option[Int]) {
		def asComparable = new ComparableOptionLong(i.asInstanceOf[Option[Long]]) // convert int to long
	}


	implicit def toComparableLongConverter(l:Long) = new ComparableLongConverter(l)

	class ComparableLongConverter(l : Long) {
		def asComparable = new ComparableOptionLong(Some(l))
	}

	implicit def toComparableOptionLongConverter(l:Option[Long]) = new ComparableOptionLongConverter(l)

	class ComparableOptionLongConverter(l : Option[Long]) {
		def asComparable = new ComparableOptionLong(l)
	}


	implicit def toComparableBooleanConverter(b:Boolean) = new ComparableBooleanConverter(b)

	class ComparableBooleanConverter(b : Boolean) {
		def asComparable = new ComparableOptionBoolean(Some(b))
	}

	implicit def toComparableOptionBooleanConverter(b:Option[Boolean]) = new ComparableOptionBooleanConverter(b)

	class ComparableOptionBooleanConverter(b : Option[Boolean]) {
		def asComparable = new ComparableOptionBoolean(b)
	}


	implicit def toComparableSeqStringConverter(s:Seq[String]) = new ComparableSeqStringConverter(s)


	class ComparableSeqStringConverter(s : Seq[String]) {
		def asComparable = new ComparableSeqString(s)
	}

	implicit def toComparableSeqIntConverter(i:Seq[Int]) = new ComparableSeqIntConverter(i)


	class ComparableSeqIntConverter(i : Seq[Int]) {
		def asComparable = new ComparableSeqLong(i.map {_.asInstanceOf[Long]})
	}



	implicit def toComparableSeqLongConverter(l:Seq[Long]) = new ComparableSeqLongConverter(l)

	class ComparableSeqLongConverter(l : Seq[Long]) {
		def asComparable = new ComparableSeqLong(l)
	}


	implicit def toComparableSeqLongConverter(b:Seq[Boolean]) = new ComparableSeqBooleanConverter(b)

	class ComparableSeqBooleanConverter(b : Seq[Boolean]) {
		def asComparable = new ComparableSeqBoolean(b)
	}

}