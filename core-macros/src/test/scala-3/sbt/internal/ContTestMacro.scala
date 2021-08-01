package sbt.internal

import sbt.internal.util.Types.Id
import sbt.internal.util.appmacro.*
import scala.quoted.*
import ConvertTestMacro.InputInitConvert
import TestObjects.ListMonadInstance

object ContTestMacro:
  inline def contMapNMacro[A](inline expr: A): List[A] =
    ${ contMapNMacroImpl('expr) }

  def contMapNMacroImpl[A: Type](expr: Expr[A])(using qctx: Quotes) =
    object ContSyntax extends Cont
    import ContSyntax.*
    val convert1: Convert[qctx.type] = new InputInitConvert(qctx)
    convert1.contMapN[A, Id](ListMonadInstance)(expr, convert1.idTransform)

end ContTestMacro
