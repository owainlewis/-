package io.forward.polyfunctors

trait Bifunctor[F[+_, +_]] {
  /**
   * Bimap applies a function to both the left and right sides of a bi structure
   */
  def bimap[A, B, C, D](domain: F[A, B], f: A => C, g: B => D): F[C, D]

  /**
   * Apply a function f: A => C over the left side only leaving the right side unchanged
   *
   */
  def <:-[A,B,C](domain: F[A,B], f: A => C) = bimap(domain, f, identity[B])
}

object Bifunctor {
  def apply[F[_, _]](implicit F: Bifunctor[F]): Bifunctor[F] = F

  implicit def Tuple2Bifunctor: Bifunctor[Tuple2] = new Bifunctor[Tuple2] {
    def bimap[A, B, C, D](domain: (A, B), f: A => C, g: B => D) = (f(domain._1), g(domain._2))
  }

  implicit def EitherBifunctor: Bifunctor[Either] = new Bifunctor[Either] {
    def bimap[A, B, C, D](domain: Either[A, B], f: A => C, g: B => D) =
      domain match {
        case Left(a)  => Left(f(a))
        case Right(b) => Right(g(b))
      }
  }
}
