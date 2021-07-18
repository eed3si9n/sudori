package sbt.internal.util.appmacro

import sbt.internal.util.Types
import scala.quoted.*

abstract class Convert:
  def convert[A: Type](using qctx: Quotes)(nme: String, in: qctx.reflect.Term): Converted[qctx.type]

  def asPredicate[A](using qctx: Quotes): (String, Type[A], qctx.reflect.Term) => Boolean =
    (n, tpe, tree) =>
      val tag = tpe
      convert(n, tree)(tag).isSuccess
end Convert

object Converted:  
  def success[C <: Quotes](using qctx: C)(
      tree: qctx.reflect.Term,
      finalTransform: qctx.reflect.Term => qctx.reflect.Term): Success[C] =
    Success(qctx)(tree, finalTransform)

  def success[C <: Quotes](using qctx: C)(tree: qctx.reflect.Term): Success[C] =
    success[C](tree, Types.idFun)

  def failure[C <: Quotes](using qctx: C)(
    position: qctx.reflect.Position,
    message: String): Failure[C] =
    Failure(qctx)(position, message)

  def notApplicable[C <: Quotes](using qctx: C): NotApplicable[C] =
    NotApplicable(qctx)

enum Converted[C <: Quotes](val qctx: C):
  def isSuccess: Boolean = this match
    case _: Success[C] => true
    case _             => false

  def transform(f: qctx.reflect.Term => qctx.reflect.Term): Converted[C] = this match
    case x: Failure[C]       => Failure(x.qctx)(x.position, x.message)
    case x: Success[C] if x.qctx == qctx =>
      Success(x.qctx)(
        f(x.tree.asInstanceOf[qctx.reflect.Term]).asInstanceOf[x.qctx.reflect.Term],
        x.finalTransform)
    case x: NotApplicable[C] => x
    case x                   => sys.error(s"Unknown case $x")

  case Success(override val qctx: C)(
      val tree: qctx.reflect.Term,
      val finalTransform: qctx.reflect.Term => qctx.reflect.Term) extends Converted[C](qctx)

  case Failure(override val qctx: C)(
      val position: qctx.reflect.Position,
      val message: String)
    extends Converted[C](qctx)

  case NotApplicable(override val qctx: C) extends Converted[C](qctx)
end Converted
