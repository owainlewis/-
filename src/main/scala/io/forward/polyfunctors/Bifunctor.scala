package io.forward.polyfunctors

trait Bifunctor[F[+_, +_]] {
  def bimap[A, B, C, D](domain: F[A, B], f: A => C, g: B => D): F[C, D]
}

object Bifunctor {
  def apply[F[_, _]](implicit F: Bifunctor[F]): Bifunctor[F] = F

  implicit def Tuple2Bifunctor: Bifunctor[Tuple2] = new Bifunctor[Tuple2] {
    def bimap[A, B, C, D](domain: (A, B), f: A => C, g: B => D) = (f(domain._1), g(domain._2))
  }

  implicit def EitherBifunctor: Bifunctor[Either] = new Bifunctor[Either] {
    def bimap[A, B, C, D](domain: Either[A, B], f: A => C, g: B => D) =
      domain match {
        case Left(a) => Left(f(a))
        case Right(b) => Right(g(b))
      }
  }
}
