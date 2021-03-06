package com.my.thing

import EvaluatorWrapperClasses._

class MacroKeypathLookup extends Lookup {

  def lookup(keypath : String, obj : A) : Comparable = MacroKeypathLookup.lookupTable.get(keypath) match {
    case None => ComparableUndefined()
    case Some(comparableGetter) => comparableGetter(obj)
  }


                              
}

object MacroKeypathLookup {

    import EvaluatorWrapperConverters._

    val lookupTable : Map[String, (A => Comparable)] = {
      val asComparable = """doesnt matter what I am as 
                            long as I'm a var defined in this method whose name is the same as
                            the method we'd like to call on each key in the map. please make 
                            me better.
                         """
                        //TODO: I need to find a much better way of doing this
                        // basically the macro is taking the actual name of the
                        // variable (in this case `asComparable`) and splicing that
                        // onto the leaf of the tree it creates for each key in the map

      CaseClassToKeypathMapMacro
        .buildNarrowAndFunctionizedMap[A,Comparable](asComparable)
    }

}