package com.my.thing

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

//props to http://stackoverflow.com/a/17224392

object Macro {

  type Keypath = String

  def apply[T](f : Any) = macro impl[T]

  def impl[T: c.WeakTypeTag](c : Context)(f: c.Tree)  = {
    import c.universe._

    def doStuff(base : c.Tree, tpe2 : Type, typePath : String = "", trees : Seq[(Keypath,Tree)] = Seq()) : Seq[(Keypath, Tree)] = {

      if (tpe2 <:< weakTypeOf[Option[_]]) { 
        //thanks http://stackoverflow.com/a/20937550
        val innerType = tpe2.baseType(weakTypeOf[Option[_]].typeSymbol) match {
          case TypeRef(_, _, targ :: Nil) => targ
          case NoType => c.abort(c.enclosingPosition, "call this method with known type parameter only.")
        }
        println ("innerType "  + innerType)

        //println(tpe2.member("map": TermName))

        val x = q"val x: $innerType"

        println(showRaw(x))

        val z = q"($x => x)"

        val stuff = doStuff(z,innerType, typePath)

        val moreStuff = q"${base}.map( $z )" 

        println("stuff " + moreStuff)

        Seq((typePath, moreStuff))

        // q"""${accessor}.map($stuff)"""

      } else if ( //grossness...maybe we should just check of its a weak type of any of the types declared in a certain package?
           tpe2 <:< weakTypeOf[AnyVal] 
        || tpe2 <:< weakTypeOf[String]
        || tpe2 <:< weakTypeOf[Seq[_]]
        ) {
        Seq((typePath, base))
      }
      else {
        val fieldTrees = tpe2.declarations.collect {
          case field if field.isMethod && field.asMethod.isCaseAccessor => {
            println(q"$base")
            println(q"$field")
            doStuff(
              q"${base}.${field}", 
              field.asMethod.returnType, typePath + "." + field.name.decoded)
          }
        }.toSeq

        trees ++ fieldTrees.flatten
        
      }

    }

    val fields = doStuff(f, weakTypeOf[T])


    val mapApply = q"Map.apply"


    val pairs = fields.map { field => 
      q"""${field._1} -> ${field._2}"""
    }

    val retval = c.Expr[Map[String, Any]](Apply(mapApply, pairs.toList))

    println("pairs" + pairs)

    //println(fields)
    println("retval" + retval)

    //c.Expr[Option[String]](fields.head)

    retval
    //c.Expr[Map[String,Any]](retval)
  }


//  def asMap[T]: Map[String, Any] = macro Macros.asMap_impl[T]

  // object Macros {

  //   def asMap_impl[T: c.WeakTypeTag](c: Context) = {
  //     import c.universe._

  //     val mapApply = Select(reify(Map).tree, newTermName("apply"))
  //     val model = Select(c.prefix.tree, newTermName("model"))

  //     val pairs = weakTypeOf[T].declarations.collect {
  //       case m: MethodSymbol if m.isCaseAccessor =>
  //         val name = c.literal(m.name.decoded)
  //         val value = c.Expr(Select(model, m.name))
  //         reify(name.splice -> value.splice).tree
  //     }

  //     c.Expr[Map[String, Any]](Apply(mapApply, pairs.toList))
  //   }
  // }
}
