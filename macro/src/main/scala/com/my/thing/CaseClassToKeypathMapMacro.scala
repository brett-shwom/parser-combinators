package com.my.thing

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

//props to http://stackoverflow.com/a/17224392

object CaseClassToKeypathMapMacro {

  type Keypath = Seq[String]

  def apply[T](f : Any) = macro impl[T]

  def impl[T: c.WeakTypeTag](c : Context)(f: c.Tree)  = {
    import c.universe._

    def explore(
      tree    : c.Tree, 
      _type   : Type, 
      keypath : Seq[String] = Seq()
      ) : Seq[(Keypath, Tree)] = {

      if (_type <:< weakTypeOf[Option[_]]) { 

        //thanks http://stackoverflow.com/a/20937550

        val optionInnerType = _type.baseType(weakTypeOf[Option[_]].typeSymbol) match {
          case TypeRef(_, _, targ :: Nil) => targ
          case NoType => c.abort(c.enclosingPosition, "call this method with known type parameter only.")
        }

        val x = q"val x: $optionInnerType"

        val subtreesAndKeypaths = explore(q"x",optionInnerType, keypath ) //TODO: do something with me

        subtreesAndKeypaths.map { case (subtreeKeypath, subtree) =>

          val lambda = q"($x => ${subtree})"

          //thanks: http://stackoverflow.com/a/17394560
          val expressionUsedForTypeChecking = c.Expr[Any](c.typeCheck(lambda))
          val lambdaTypeArguments = expressionUsedForTypeChecking.actualType.typeArgs
          val lambdaReturnType = lambdaTypeArguments.last //kind of an ugly way to get the return type of the lambda

          val mapOperation = 
            if (lambdaReturnType <:< weakTypeOf[Option[_]]) q"${tree}.flatMap( $lambda )"
            else                                            q"${tree}.map( $lambda )"

          (subtreeKeypath, mapOperation)

        }



      } else if ( //grossness...maybe we should just check of its a weak type of any of the types declared in a certain package?
           _type <:< weakTypeOf[AnyVal] 
        || _type <:< weakTypeOf[String]
        || _type <:< weakTypeOf[Seq[_]]
        ) {
        Seq((keypath, tree))
      }
      else {
        val subtrees = _type.declarations.collect {
          case field if field.isMethod && field.asMethod.isCaseAccessor => {
            explore(
              q"${tree}.${field}", 
              field.asMethod.returnType, 
              keypath :+ field.name.decoded
            )
          }
        }.toSeq

        subtrees.flatten
        
      }

    }

    val keypathsAndFields = explore(f, weakTypeOf[T])

    val pairs = keypathsAndFields
                  .map {  case (keypath, tree) => (keypath.mkString("."), tree) }
                  .map {  case (keypath, tree) => q"""${keypath} -> ${tree}"""  }

    c.Expr[Map[String, Any]](Apply(q"Map.apply", pairs.toList))


  }
}
