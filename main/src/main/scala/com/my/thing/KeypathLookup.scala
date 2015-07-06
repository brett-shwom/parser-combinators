package com.my.thing

import EvaluatorWrapperClasses._


//TODO: I should take generic type parameters...
trait Lookup {
  def lookup(keypath : String, obj : A) : Comparable
}

// object KeypathLookup extends ManualKeypathLookup
object KeypathLookup extends MacroKeypathLookup