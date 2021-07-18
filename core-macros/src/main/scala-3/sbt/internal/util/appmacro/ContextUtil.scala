package sbt.internal.util
package appmacro

import scala.quoted.*

object ContextUtil

class ContextUtil[C <: Quotes](using val qctx: C):
  import qctx.reflect.*

  /**
   * Substitutes wrappers in tree `t` with the result of `subWrapper`.
   * A wrapper is a Tree of the form `f[T](v)` for which isWrapper(<Tree of f>, <Underlying Type>, <qual>.target) returns true.
   * Typically, `f` is a `Select` or `Ident`.
   * The wrapper is replaced with the result of `subWrapper(<Type of T>, <Tree of v>, <wrapper Tree>)`
   */
  def transformWrappers(
      tree: Term,
      subWrapper: (String, Type[_], Term, Term) => Converted[qctx.type]
  ): Term =
    // the main tree transformer that replaces calls to InputWrapper.wrap(x) with
    //  plain Idents that reference the actual input value
    object appTransformer extends TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case Apply(TypeApply(Select(_, nme), targ :: Nil), qual :: Nil) =>
            subWrapper(nme, targ.tpe.asType, qual, tree) match
              case x: Converted.Success[C @unchecked] if qctx == x.qctx =>
                x.finalTransform(x.tree)
              case x: Converted.Failure[C @unchecked] =>
                report.error(x.message, x.position)
                sys.error("macro error: " + x.message)
              case _ =>
                super.transformTerm(tree)(owner)
          case _ =>
            super.transformTerm(tree)(owner)

    end appTransformer
    appTransformer.transformTerm(tree)(Symbol.spliceOwner)
end ContextUtil
