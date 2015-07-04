package com.my.thing

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

//props to http://stackoverflow.com/a/17224392

object Macro {

  def apply[T](f : Any) = macro impl[T]

  def impl[T: c.WeakTypeTag](c : Context)(f: c.Tree)  = {
    import c.universe._

    val tpe = weakTypeOf[T]

    def doStuff(tpe2 : Type, accessor: c.Symbol, typePath : String) : Tree = {


      println(tpe2 <:< weakTypeOf[AnyVal])

      if (tpe2 <:< weakTypeOf[Option[_]]) {
        //thanks http://stackoverflow.com/a/20937550
        val innerType = tpe2.baseType(weakTypeOf[Option[_]].typeSymbol) match {
          case TypeRef(_, _, targ :: Nil) => targ
          case NoType => c.abort(c.enclosingPosition, "call this method with known type parameter only.")
        }
        println ("innerType "  + innerType)

        println(tpe2.member("map": TermName))

        val stuff = doStuff(innerType, q"(x=>x)", typePath)

        val moreStuff = q"${accessor}.map(${stuff})"

        println("stuff " + moreStuff)

        moreStuff

        // q"""${accessor}.map($stuff)"""

      } else if ( //grossness...maybe we should just check of its a weak type of any of the types declared in a certain package?
           tpe2 <:< weakTypeOf[AnyVal] 
        || tpe2 <:< weakTypeOf[String]
        || tpe2 <:< weakTypeOf[Seq[_]]
        ) {
        q"$accessor"
      }
      else {
        q"$accessor"
      }

      // val fields = tpe2.declarations.collect {
      //   case field if field.isMethod && field.asMethod.isCaseAccessor => {
      //     field.asMethod.accessed
      //   }
      // }

      // //this comparison seems weird....

      // if (fields.size == 0) Seq(typePath)
      // else {
      //   fields.map {
      //     field => s"""${typePath}.${field.name.decoded}"""
      //   }
      // }

    }

    val fields = tpe.declarations.collect {
      case field if field.isMethod && field.asMethod.isCaseAccessor => {
        doStuff(field.asMethod.returnType,field.asMethod.accessed, field.name.decoded)
      }
    }

    val flattened = fields



    println(flattened)

    // val mapApply = Select(reify(Map).tree, newTermName("apply"))


    // val pairs = flattened.map { field => 
    //   val s = Select(f, field)
    //   q"${field} -> ${s}"
    // }

    //val retval = c.Expr[Map[String, Any]](Apply(mapApply, pairs.toList))

    println(fields)

    c.Expr(q"..$fields")

    
    //c.Expr[Map[String,String]](q"""Map("a" -> "a")""")
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
