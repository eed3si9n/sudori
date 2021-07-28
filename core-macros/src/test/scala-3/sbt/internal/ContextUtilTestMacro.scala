package sbt.internal

import sbt.internal.util.appmacro.*
import scala.quoted.*
import TestObjects.ListMonadInstance

object ContextUtilTestMacro:
  inline def extractTypeCon(inline expr: Boolean): String =
    ${ extractTypeConImpl('expr) }

  inline def extractSingleton(inline expr: Boolean): String =
    ${ extractSingletonImpl('expr) }

  inline def makeLambda(inline expr: Unit): Boolean => String =
    ${ makeLambdaImpl('expr) }

  inline def makeLambda2(inline expr: Unit): Int => String =
    ${ makeLambdaImpl2('expr) }

  def extractTypeConImpl(expr: Expr[Boolean])(using qctx: Quotes) =
    val util: ContextUtil[qctx.type] = new ContextUtil(qctx) {}
    val typeCon = util.extractTypeCon(Demo, "M")
    Expr(typeCon.toString)

  def extractSingletonImpl(expr: Expr[Boolean])(using qctx: Quotes) =
    val util: ContextUtil[qctx.type] = new ContextUtil(qctx) {}
    import util.qctx.reflect.*
    def doExtract(
        i: MonadInstance & scala.Singleton
    )(using itpe: Type[i.type]): Expr[String] =
      val r = util.extractSingleton[i.type]
      Expr(r.asTerm.toString)
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

  def makeLambdaImpl2(expr: Expr[Unit])(using qctx: Quotes) =
    import qctx.reflect.*
    val ANON_FUN = "$anonfun"
    val funSym = Symbol.newMethod(
      parent = Symbol.spliceOwner,
      name = ANON_FUN,
      tpe = MethodType(List("x"))(_ => List(TypeRepr.of[Int]), _ => TypeRepr.of[String]),
      flags = Flags.Synthetic,
      privateWithin = Symbol.noSymbol,
    )
    val funDef = DefDef(
      symbol = funSym,
      rhsFn = (paramss: List[List[Tree]]) => {
        val param = paramss.head.head
        val toStr = Select.unique(Ref(param.symbol), "toString")
        Option(toStr.appliedToNone)
      }
    )
    Block(
      List(funDef),
      Closure(Ref(funSym), None)
    ).asExprOf[Int => String]

  object Demo:
    type M[x] = List[x]
  end Demo
end ContextUtilTestMacro
