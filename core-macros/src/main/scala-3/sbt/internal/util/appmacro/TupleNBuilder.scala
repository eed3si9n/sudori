package sbt.internal.util.appmacro

import scala.quoted.*

trait TupleNBuilder[C <: Quotes & scala.Singleton](override val qctx: C) extends TupleBuilder[C]:
  self: ContextUtil[C] =>

  import qctx.reflect.*

  override def makeTuple(inputs: List[Input]): BuilderResult =
    new BuilderResult {
      override def representationC: TypeLambda =
        TypeLambda(
          paramNames = List("F1"),
          boundsFn = _ =>
            List(
              TypeBounds.upper(
                TypeLambda(
                  paramNames = List("a"),
                  boundsFn = _ => List(TypeBounds.empty),
                  bodyFn = _ => TypeRepr.of[Any],
                )
              )
            ),
          bodyFn = (tl: TypeLambda) =>
            val F1: TypeRepr = tl.param(0)
            val tupleTypeArgs = inputs.map(in => F1.appliedTo(in.tpe))
            tupleType(tupleTypeArgs)
        )

      override def tupleTerm: Term =
        mkTuple(inputs.map(in => in.expr))
    }
end TupleNBuilder
