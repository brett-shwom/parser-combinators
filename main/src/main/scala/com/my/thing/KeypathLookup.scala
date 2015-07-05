package com.my.thing

import EvaluatorWrapperClasses._

trait Lookup {
	def lookup(keypath : String, obj : A) : Comparable
}

object KeypathLookup extends ManualKeypathLookup {


	// import EvaluatorWrapperConverters._


	// //build using 
	// //  - macros
	// //  - jit compilation?

	// val lookupTable : Map[String, A => Comparable] = Map(
	// 	"a.f1"    -> (a => a.f1.asComparable),
	// 	"a.f2"    -> (a => a.f2.asComparable), 
	// 	"a.f3.f1" -> (a => a.f3.f1.asComparable),
	// 	"a.f4"    -> (a => a.f4.asComparable)
	// )

	// def lookup(keypath : String, obj : A) : Comparable = lookupTable.get(keypath) match {
	// 	case None => ComparableUndefined()
	// 	case Some(getField) => getField(obj)
	// }

}