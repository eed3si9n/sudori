package sbt.internal.util.appmacro

import scala.quoted.*

trait ContextUtil[C <: Quotes & Singleton](val qctx: C):
  import qctx.reflect.*
  given qctx.type = qctx

  private var counter: Int = -1
  def freshName(prefix: String): String =
    counter = counter + 1
    s"${prefix}_synth${counter}"
end ContextUtil
