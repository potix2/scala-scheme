package com.potix2


import scalaz._
import scalaz.effect.{IO, IORef, MonadIO}

package object scheme {
  type Env = IORef[List[(String, IORef[LispVal])]]
  type ThrowsError[+A] = \/[LispError, A]
  type IOThrowsError[+A] = EitherT[IO,LispError,A]

  implicit def liftIO = new MonadIO[IOThrowsError]{
    override def liftIO[A](ioa: IO[A]): IOThrowsError[A] = EitherT.right(ioa)
    override def point[A](a: => A): IOThrowsError[A] = EitherT.right(IO(a))
    override def bind[A, B](fa: IOThrowsError[A])(f: (A) => IOThrowsError[B]): IOThrowsError[B] = fa.flatMap(f)
  }
}
