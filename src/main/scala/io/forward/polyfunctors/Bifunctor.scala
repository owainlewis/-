package io.forward.polyfunctors

trait Bifunctor[F[+_, +_]] {
  def bimap[A, B, C, D](k: F[A, B], f: A => C, g: B => D): F[C, D]
}

object Bifunctor {

  def apply[F[_, _]](implicit F: Bifunctor[F]): Bifunctor[F] = F

  implicit def Tuple2Bifunctor: Bifunctor[Tuple2] = new Bifunctor[Tuple2] {
    def bimap[A, B, C, D](k: (A, B), f: A => C, g: B => D) = (f(k._1), g(k._2))
  }

  implicit def EitherBifunctor: Bifunctor[Either] = new Bifunctor[Either] {
    def bimap[A, B, C, D](k: Either[A, B], f: A => C, g: B => D) =
      k match {
        case Left(a) => Left(f(a))
        case Right(b) => Right(g(b))
      }
  }
}

object BifunctorExamples {

  val inc = (x: Int) => x + 1

  Bifunctor[Tuple2].bimap((1,2), Foo.inc, Foo.inc)
}

