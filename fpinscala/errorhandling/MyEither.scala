package fpinscala.errorhandling

sealed trait MyEither[+E, +A] {
    def map[B](f: A => B): MyEither[E, B] = this match {
        case Left(v) => Left(v)
        case Right(v) => Right(f(v))
    }
    
    def flatMap[F >: E, B](f: A => MyEither[F, B]): MyEither[F, B] = this match {
        case Left(v) => Left(v)
        case Right(v) => f(v)
    }
}

case class Left[+E](value: E) extends MyEither[E, Nothing]

case class Right[+A](value: A) extends MyEither[Nothing, A]

