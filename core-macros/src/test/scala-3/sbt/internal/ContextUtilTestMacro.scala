package sbt.internal

import sbt.internal.util.appmacro.*
import scala.quoted.*
import TestObjects.ListMonadInstance

object ContextUtilTestMacro:
  inline def extractTypeCon(inline expr: Boolean): String =
    ${ extractTypeConImpl('expr) }

  inline def extractInstance(inline expr: Boolean): String =
    ${ extractInstanceImpl('expr) }

  inline def makeLambda(inline expr: Unit): Boolean => String =
    ${ makeLambdaImpl('expr) }

  def extractTypeConImpl(expr: Expr[Boolean])(using qctx: Quotes) =
    val util: ContextUtil[qctx.type] = new ContextUtil(qctx) {}
    val typeCon = util.extractTypeCon(Demo, "M")
    Expr(typeCon.toString)

  def extractInstanceImpl(expr: Expr[Boolean])(using qctx: Quotes) =
    val util: ContextUtil[qctx.type] = new ContextUtil(qctx) {}
    import util.qctx.reflect.*
    def doExtract(
        i: MonadInstance & scala.Singleton
    )(using itpe: Type[i.type]): Expr[String] =
      val r = util.extractInstance(TypeRepr.of[i.type])
      Expr(r.toString)
    doExtract(ListMonadInstance)

  def makeLambdaImpl(expr: Expr[Unit])(using qctx: Quotes) =
    import qctx.reflect.*
    Lambda(
      owner = Symbol.spliceOwner,
      tpe = MethodType(List("x"))(_ => List(TypeRepr.of[Boolean]), mt => TypeRepr.of[String]),
      rhsFn = (sym, params0) => {
        val param = params0.head
        val toStr = Select.unique(Ref(param.symbol), "toString")
        toStr.appliedToNone
      }
    ).asExprOf[Boolean => String]

  object Demo:
    type M[x] = List[x]
  end Demo
end ContextUtilTestMacro
