package sbt.internal.util.appmacro

import sbt.internal.util.Types.Id
import scala.quoted.*

trait ContextUtil[C <: Quotes & Singleton](val qctx: C):
  import qctx.reflect.*
  given qctx.type = qctx

  /**
   * Returns a Type representing the type constructor tcp.<name>. For example, given `object Demo {
   * type M[x] = List[x] }`, the call `extractTypeCon(Demo, "M")` will return a type representing
   * the type constructor `[x] List[x]`.
   */
  def extractTypeCon(tcp: AnyRef & scala.Singleton, name: String)(using
      tcpt: Type[tcp.type]
  ): TypeRepr =
    val tcpTpe = TypeRepr.of[tcp.type]
    val fSym = tcpTpe.typeSymbol.declaredType(name).head
    val typeConTpe: TypeRepr = tcpTpe.memberType(fSym)
    val hiRepr = typeConTpe match
      case TypeBounds(low, TypeLambda(_, _, repr)) => repr
    hiRepr

  private var counter: Int = -1
  def freshName(prefix: String): String =
    counter = counter + 1
    s"${prefix}_synth${counter}"

  trait TermTransform[F[_]]:
    def apply(in: Term): Term
  end TermTransform

  def idTransform: TermTransform[Id] = in => in
end ContextUtil
