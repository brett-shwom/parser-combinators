package com.my.thing

object Evaluator {

	case class R(s : S, j : String) //extends Model
  case class S(b : Seq[Int]) //extends Model

  // val tb = runtimeMirror(getClass.getClassLoader).mkToolBox()

  // val compiledCode = toolbox.compile(code)

}

// import scala.reflect.macros.whitebox.Context
// import scala.language.experimental.macros

// trait Model

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
//           println(value.asMap_impl)
//           reify(name.splice -> value.splice).tree
//       }

//       c.Expr[Map[String, Any]](Apply(mapApply, pairs.toList))
//     }
//   }
// }
