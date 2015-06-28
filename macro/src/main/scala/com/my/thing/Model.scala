package com.my.p

// import scala.reflect.macros.whitebox.Context
// import scala.language.experimental.macros

// trait Model

// //props to http://stackoverflow.com/a/17224392

// object Model {
//   implicit class Mappable[M <: Model](val model: M) extends AnyVal {
//     def asMap: Map[String, Any] = macro Macros.asMap_impl[M]
//   }

//   object Macros {

//     def asMap_impl[T: c.WeakTypeTag](c: Context) = {
//       import c.universe._

//       val mapApply = Select(reify(Map).tree, newTermName("apply"))
//       val model = Select(c.prefix.tree, newTermName("model"))

//       val pairs = weakTypeOf[T].declarations.collect {
//         case m: MethodSymbol if m.isCaseAccessor =>
//           val name = c.literal(m.name.decoded)
//           val value = c.Expr(Select(model, m.name))
//           reify(name.splice -> value.splice).tree
//       }

//       c.Expr[Map[String, Any]](Apply(mapApply, pairs.toList))
//     }
//   }
// }
