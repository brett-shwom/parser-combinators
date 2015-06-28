package com.my.thing

object EvaluatorWrapperClasses {

	sealed abstract class Comparable(value : Any)

	case class ComparableString(value : String) extends Comparable(value)
	case class ComparableInt(value : Int) extends Comparable(value)
	case class ComparableLong(value : Long) extends Comparable(value)
	case class ComparableBoolean(value : Boolean) extends Comparable(value)

	//TODO: should we have support for float?

	case class ComparableOptionString(value : Option[String]) extends Comparable(value)
	case class ComparableOptionInt(value : Option[Int]) extends Comparable(value)
	case class ComparableOptionLong(value : Option[Long]) extends Comparable(value)
	case class ComparableOptionBoolean(value : Option[Boolean]) extends Comparable(value)

	case class ComparableSeqInt(value : Seq[Int]) extends Comparable(value)
	case class ComparableSeqLong(value : Seq[Long]) extends Comparable(value)
	case class ComparableSeqString(value : Seq[String]) extends Comparable(value)
	case class ComparableSeqBoolean(value : Seq[Boolean]) extends Comparable(value)

	case class ComparableUndefined() extends Comparable(None)

}

object EvaluatorWrapperClassesConversions {

	import EvaluatorWrapperClasses._

	implicit def comparableStringWrapper(s: String) = 
		new ComparableString(s)

	implicit def comparableIntWrapper(i: Int) = 
		new ComparableInt(i)

	implicit def ComparableLongWrapper(l: Long) = 
		new ComparableLong(l)

	implicit def ComparableBooleanWrapper(b: Boolean) = 
		new ComparableBoolean(b)


	implicit def comparableOptionStringWrapper(so: Option[String]) = 
		new ComparableOptionString(so)

	implicit def comparableOptionIntWrapper(ii: Option[Int]) = 
		new ComparableOptionInt(ii)

	implicit def ComparableOptionLongWrapper(ll: Option[Long]) = 
		new ComparableOptionLong(ll)

	implicit def ComparableOptionBooleanWrapper(bb: Option[Boolean]) = 
		new ComparableOptionBoolean(bb)


	implicit def ComparableSeqStringWrapper(ss: Seq[String]) = 
		new ComparableSeqString(ss)

	implicit def ComparableSeqIntWrapper(si: Seq[Int]) = 
		new ComparableSeqInt(si)

	implicit def ComparableSeqLongWrapper(sl: Seq[Long]) = 
		new ComparableSeqLong(sl)

	implicit def ComparableSeqBooleanWrapper(sb: Seq[Boolean]) = 
		new ComparableSeqBoolean(sb)
}