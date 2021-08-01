package sbt.internal

import sbt.internal.util.appmacro.*
import scala.quoted.*

object ConvertTestMacro:
  final val WrapInitName = "wrapInit"
  final val WrapInitTaskName = "wrapInitTask"

  inline def someMacro(inline expr: Boolean): Boolean =
    ${ someMacroImpl('expr) }

  def someMacroImpl(expr: Expr[Boolean])(using qctx: Quotes) =
    val convert1: Convert[qctx.type] = new InputInitConvert(qctx)
    import convert1.qctx.reflect.*
    def addTypeCon(tpe: TypeRepr, qual: Term, selection: Term): Term =
      tpe.asType match
        case '[a] =>
          '{
            Option[a](${ selection.asExprOf[a] })
          }.asTerm
    def substitute(name: String, tpe: TypeRepr, qual: Term, replace: Term) =
      convert1.convert[Boolean](name, qual) transform { (tree: Term) =>
        addTypeCon(tpe, tree, replace)
      }
    convert1.transformWrappers(expr.asTerm, substitute).asExprOf[Boolean]

  class InputInitConvert[C <: Quotes & scala.Singleton](override val qctx: C)
      extends Convert[C](qctx)
      with ContextUtil[C](qctx):
    import qctx.reflect.*
    def convert[A: Type](nme: String, in: Term): Converted =
      nme match
        case WrapInitName     => Converted.success(in)
        case WrapInitTaskName => Converted.Failure(in.pos, initTaskErrorMessage)
        case _                => Converted.NotApplicable()

    private def initTaskErrorMessage = "Internal sbt error: initialize+task wrapper not split"
  end InputInitConvert
end ConvertTestMacro
