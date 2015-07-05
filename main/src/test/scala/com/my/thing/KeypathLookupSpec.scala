package com.my.thing

import org.scalatest._

import EvaluatorWrapperClasses._

class KeypathLookupSpec extends FlatSpec with Matchers {

  "The keypath lookup" should "resolve properties of properties and subproperties of A's" in {

    import EvaluatorWrapperConverters._

    val a = A("f1", Seq(1), B("bf1"))

    KeypathLookup.lookup("a.f1", a) should equal("f1".asComparable)
    KeypathLookup.lookup("a.f2", a) should equal(Seq(1).asComparable)
    KeypathLookup.lookup("a.f3.f1", a) should equal("bf1".asComparable)
    KeypathLookup.lookup("z.zzz", a) should equal(ComparableUndefined())

  }

  

}

