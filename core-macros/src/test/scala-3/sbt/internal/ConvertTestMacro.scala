package sbt.internal

import sbt.internal.util.appmacro.*
import scala.quoted.*

object ConvertTestMacro:
  final val WrapInitName = "wrapInit"
  final val WrapInitTaskName = "wrapInitTask"

  inline def someMacro(inline expr: Boolean): Boolean =
    ${ someMacroImpl('expr) }

  def someMacroImpl(expr: Expr[Boolean])(using qctx: Quotes) =
    import quotes.reflect.*
    val convert1 = InputInitConvert
    val util = new ContextUtil[qctx.type]()

    def addTypeCon(tpe: Type[_], qual: Term, selection: Term): Term =
      tpe match
        case '[a] =>
          '{
            Option(${selection.asExprOf[a]})
          }.asTerm

    def sub(name: String, tpe: Type[_], qual: Term, replace: Term) =
      convert1.convert[Boolean](name, qual) transform { (tree: Term) =>
        addTypeCon(tpe, tree, replace)
      }
    val retval = util.transformWrappers(expr.asTerm, sub)
    retval.asExprOf[Boolean]

  object InputInitConvert extends Convert:
    def convert[A: Type](using qctx: Quotes)(nme: String, in: qctx.reflect.Term): Converted[qctx.type] =
      nme match
        case WrapInitName     => Converted.success(in)
        case WrapInitTaskName => Converted.failure(in.pos, initTaskErrorMessage)
        case _                => Converted.notApplicable

    private def initTaskErrorMessage = "Internal sbt error: initialize+task wrapper not split"
  end InputInitConvert
end ConvertTestMacro
