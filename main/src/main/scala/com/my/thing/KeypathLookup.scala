package com.my.thing

import EvaluatorWrapperClasses._

object KeypathLookup {

	//build using 
	//  - macros
	//  - jit compilation?
	val lookupTable : Map[String, A => Any] = Map(
		"f1" -> (a => a.f1),
		"f2" -> (a => a.f2), 
		"f3.f1" -> (a => a.f3.f1)
	)

	def lookup(keypath : String, obj : A) = lookupTable.get(keypath) match {
		case None => ComparableUndefined()
		case Some(getField) => getField(obj)
	}

}