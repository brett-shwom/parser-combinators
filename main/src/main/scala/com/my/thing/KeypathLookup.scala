package com.my.thing

import EvaluatorWrapperClasses._

trait Lookup {
  def lookup(keypath : String, obj : A) : Comparable
}

// object KeypathLookup extends ManualKeypathLookup
object KeypathLookup extends MacroKeypathLookup