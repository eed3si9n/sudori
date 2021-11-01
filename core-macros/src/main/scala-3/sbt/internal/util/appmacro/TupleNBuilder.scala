package sbt.internal.util.appmacro

import scala.quoted.*

trait TupleNBuilder[C <: Quotes & scala.Singleton](override val qctx: C) extends TupleBuilder[C]:
  self: ContextUtil[C] =>

  import qctx.reflect.*

  override def makeTuple(inputs: List[Input]): BuilderResult =
    new BuilderResult {
      override def inputTupleTypeRepr: TypeRepr =
        tupleTypeRepr(inputs.map(_.tpe))
      override def tupleExpr: Expr[Tuple] =
        Expr.ofTupleFromSeq(inputs.map(_.term.asExpr))
    }
end TupleNBuilder
