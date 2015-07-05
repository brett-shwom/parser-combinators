package com.my.thing

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

//props to http://stackoverflow.com/a/17224392

object CaseClassToKeypathMapMacro {

  type Keypath = Seq[String]

  //def buildWideMap[T](f : Any) : Map[String, Any] = macro buildWideMapImpl[T]
  def apply[T](f : Any) : Map[String, Any] = macro buildWideMapImpl[T]

  def buildNarrowMap[T,R](f : Any, narrowingFunctionIdentifier : String) : Map[String,R] = macro buildNarrowMapImpl[T,R]

  def buildNarrowMapImpl[T: c.WeakTypeTag, R:c.WeakTypeTag](c : Context)(f: c.Tree, narrowingFunctionIdentifier : c.Tree)  
    = {
    
    import c.universe._

    //TODO: I could replace this with TermName(someString) if I knew how to pass a String (not a Tree) to a macro method...

    val narrowingFunctionTermName = narrowingFunctionIdentifier match { //TODO: how can I do this with quasiquote unlifting?
      case Ident(termName@TermName(x)) => termName
      case _ => c.abort(c.enclosingPosition, "Not an Ident(TermName())")
    }

    val keyValuePairs = buildMapKeypathTreePairs[T](c)(f)
                          .map {  case (keypath, tree) => q"""${keypath} -> ${tree}.${narrowingFunctionTermName}"""  }

    q"Map.apply(..${keyValuePairs})"


  }

  def buildWideMapImpl[T: c.WeakTypeTag](c : Context)(f: c.Tree) = {
    import c.universe._

    val keyValuePairs = buildMapKeypathTreePairs[T](c)(f)
                          .map {  case (keypath, tree) => q"""${keypath} -> ${tree}"""}

    q"Map.apply(..${keyValuePairs})"
  }


  def buildMapKeypathTreePairs[T: c.WeakTypeTag](c : Context)(f: c.Tree)  = {
    import c.universe._

    def explore(
      tree    : c.Tree, 
      _type   : Type, //TODO: is there a way to remove this `_type` parameter and instead get the type it by calling some method on `f`? ... maybe this makes no sense?
      keypath : Seq[String] = Seq()
      ) : Seq[(Keypath, Tree)] = {

      if (_type <:< weakTypeOf[Option[_]]) { 

        //thanks http://stackoverflow.com/a/20937550

        val optionInnerType = _type.baseType(weakTypeOf[Option[_]].typeSymbol) match {
          case TypeRef(_, _, targ :: Nil) => targ
          case NoType => c.abort(c.enclosingPosition, "call this method with known type parameter only.")
        }

        val x = q"val x: $optionInnerType"

        val subtreesAndKeypaths = explore(q"x",optionInnerType, keypath )

        subtreesAndKeypaths.map { case (subtreeKeypath, subtree) =>

          val lambda = q"($x => ${subtree})"

          //thanks: http://stackoverflow.com/a/17394560
          val expressionUsedForTypeChecking = c.Expr[Any](c.typecheck(lambda))
          val lambdaTypeArguments = expressionUsedForTypeChecking.actualType.typeArgs
          val lambdaReturnType = lambdaTypeArguments.last //kind of an ugly way to get the return type of the lambda

          val possiblyMappedOrFlatMappedTree = 
            if (lambdaReturnType <:< weakTypeOf[Option[_]]) q"${tree}.flatMap( $lambda )"
            else if (lambda equalsStructure q"($x => x)")   tree
            else                                            q"${tree}.map( $lambda )"
            //N.B.  I check if the lambda is just the identity lambda (x=>x), in which case there's no need to map
            //      This avoids creating trees which have an identity lambda as a leaf node.
            //      Ex: (Map.apply("anOptionInt".$minus$greater(a.anOptionInt.map(((x: Int) => x)))))
            //           could be written as
            //          (Map.apply("anOptionInt".$minus$greater(a.anOptionInt))

          (subtreeKeypath, possiblyMappedOrFlatMappedTree)

        }

      } else if ( //TODO: grossness...maybe we should just check of its a weak type of any of the types declared in a certain package?
           _type <:< weakTypeOf[AnyVal] 
        || _type <:< weakTypeOf[String]
        || _type <:< weakTypeOf[Seq[_]]
        ) {
        Seq((keypath, tree))
      }
      else {
        val subtrees = _type.decls.collect {
          case field if field.isMethod && field.asMethod.isCaseAccessor => {
            explore(
              q"${tree}.${field}", 
              field.asMethod.returnType, 
              keypath :+ field.name.decodedName.toString
            )
          }
        }.toSeq

        subtrees.flatten
        
      }
    }

    val keypathsAndFields = explore(f, weakTypeOf[T])

    val keypathTreePairs = keypathsAndFields
                          .map {  case (keypath, tree) => (keypath.mkString("."), tree) }


    keypathTreePairs

  }
}
