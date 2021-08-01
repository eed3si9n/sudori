package sbt.internal.util.appmacro

trait Zero[A]:
  def zero: A

object Zero extends LowPriorityZero:
  private[appmacro] def mk[A](a: A): Zero[A] = new Zero:
    def zero: A = a

  given Zero[Byte] = Zero.mk(0: Byte)
  given Zero[Char] = Zero.mk(0: Char)
  given Zero[Short] = Zero.mk(0: Short)
  given Zero[Int] = Zero.mk(0)
  given Zero[Long] = Zero.mk(0L)
  given Zero[Float] = Zero.mk(0f)
  given Zero[Double] = Zero.mk(0.0)
  given Zero[Boolean] = Zero.mk(false)
  given Zero[Unit] = Zero.mk((): Unit)
  given Zero[String] = Zero.mk("")

class LowPriorityZero:
  given [A]: Zero[A] = Zero.mk(null.asInstanceOf[A])
