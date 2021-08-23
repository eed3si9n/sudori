package sbt.internal.util
package appmacro

import sbt.internal.util.Types.Id

// import sbt.internal.util.Classes.Applicative

/**
 * A separate hierarchy of Applicative/Monad.
 */
trait Instance:
  type F[x]

  def mapN[Tup <: Tuple, A2](in: Tuple.Map[Tup, F], f: Tup => A2): F[A2]
  def map[A1, A2](in: F[A1], f: A1 => A2): F[A2]
  def pure[A](in: () => A): F[A]
end Instance

trait MonadInstance extends Instance:
  def flatten[A](in: F[F[A]]): F[A]
end MonadInstance
