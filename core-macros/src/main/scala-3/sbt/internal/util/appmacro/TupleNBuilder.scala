package sbt.internal.util.appmacro

import scala.quoted.*

trait TupleNBuilder[C <: Quotes & scala.Singleton](override val qctx: C) extends TupleBuilder[C]:
  self: ContextUtil[C] =>

  import qctx.reflect.*

  override def makeTuple(inputs: List[Input]): BuilderResult =
    new BuilderResult {
      override def representationC: TypeRepr =
        tupleType(inputs.map(_.tpe))
      override def tupleTerm: Term =
        mkTuple(inputs.map(in => in.expr))
    }
end TupleNBuilder
