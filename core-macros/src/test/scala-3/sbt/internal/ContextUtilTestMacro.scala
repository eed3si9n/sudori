package sbt.internal

import sbt.internal.util.appmacro.*
import scala.quoted.*

object ContextUtilTestMacro:
  inline def someMacro(inline expr: Boolean): String =
    ${ someMacroImpl('expr) }

  def someMacroImpl(expr: Expr[Boolean])(using qctx: Quotes) =
    val util: ContextUtil[qctx.type] = new ContextUtil(qctx) {}
    val typeCon = util.extractTypeCon(Demo, "M")
    Expr(typeCon.toString)

  object Demo:
    type M[x] = List[x]
  end Demo
end ContextUtilTestMacro
