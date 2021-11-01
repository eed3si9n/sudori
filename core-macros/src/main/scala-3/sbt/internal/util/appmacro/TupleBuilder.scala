package sbt.internal.util.appmacro

import scala.quoted.*

/**
 * A `TupleBuilder` abstracts the work of constructing a tuple data structure such as a `TupleN` or
 * `KList` and extracting values from it. The `Instance` macro implementation will (roughly)
 * traverse the tree of its argument and ultimately obtain a list of expressions with type `M[T]`
 * for different types `T`. The macro constructs an `Input` value for each of these expressions that
 * contains the `Type` for `T`, the `Tree` for the expression, and a `ValDef` that will hold the
 * value for the input.
 *
 * `TupleBuilder.apply` is provided with the list of `Input`s and is expected to provide three
 * values in the returned BuilderResult. First, it returns the constructed tuple data structure Tree
 * in `input`. Next, it provides the type constructor `representationC` that, when applied to F,
 * gives the type of tuple data structure. For example, a builder that constructs a `Tuple3` for
 * inputs `F[Int]`, `F[Boolean]`, and `F[String]` would provide a Type representing `[L[x]] (L[Int],
 * L[Boolean], L[String])`. The `input` method would return a value whose type is that type
 * constructor applied to F, or `(F[Int], F[Boolean], F[String])`.
 *
 * Finally, the `extract` method provides a list of vals that extract information from the applied
 * input. The type of the applied input is the type constructor applied to `Id` (`[X] X`). The
 * returned list of ValDefs should be the ValDefs from `inputs`, but with non-empty right-hand
 * sides.
 */
trait TupleBuilder[C <: Quotes & scala.Singleton](val qctx: C):
  self: ContextUtil[C] =>

  import qctx.reflect.*

  def makeTuple(inputs: List[Input]): BuilderResult

  trait BuilderResult:
    def inputTupleTypeRepr: TypeRepr
    def tupleExpr: Expr[Tuple]
  end BuilderResult
end TupleBuilder
