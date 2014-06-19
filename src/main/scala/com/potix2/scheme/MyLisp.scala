package com.potix2.scheme

import scalaz._
import Scalaz._
import scalaz.effect.{IO, IORef, MonadIO}

object Lisp {
  type Env = IORef[List[(String, IORef[LispVal])]]
  type ThrowsError[+A] = \/[LispError, A]
  type IOThrowsError[+A] = EitherT[IO,LispError,A]

  implicit def liftIO = new MonadIO[IOThrowsError]{
    override def liftIO[A](ioa: IO[A]): IOThrowsError[A] = EitherT.right(ioa)
    override def point[A](a: => A): IOThrowsError[A] = EitherT.right(IO(a))
    override def bind[A, B](fa: IOThrowsError[A])(f: (A) => IOThrowsError[B]): IOThrowsError[B] = fa.flatMap(f)
  }
}

object MyLisp extends App
with LispParser
with IOPrimitives
with ListPrimitives
with Evaluator
with LispEnv {
  import Lisp._

  def readPrompt(prompt: String): IO[String] = for {
    _ <- IO.putStr(prompt)
    line <- IO.readLn
  } yield line

  def evalString(env: Env)(expr: String): IO[String] = runIOThrows(
    for {
      e <- liftThrows(readExpr(expr))
      evaled <- eval(env)(e)
    } yield evaled.toString)

  def evalAndPrint(env: Env)(expr: String): IO[Unit] = for {
    p <- evalString(env)(expr)
    _ <- IO.putStrLn(p)
  } yield ()

  def until_[A](pred: A => Boolean)(prompt: IO[A])(action: A => IO[Unit]): IO[Unit] = for {
    result <- prompt
  } yield if (pred(result))
      ()
    else
      (for {
        _ <- action(result)
        _ <- until_(pred)(prompt)(action)
      } yield ()).unsafePerformIO()

  def runOne(args: Array[String]): IO[Unit] = for {
    env <- (primitiveBindings >>= FuncUtil.flip(bindVars)(List(("args", LispList(args.drop(1).map(LispString(_)).toList)))))
    result <- runIOThrows(eval(env)(LispList(List(LispAtom("load"), LispString(args(0))))).map(_.toString))
    _ <- IO.putStrLn(result)
  } yield ()

  def runRepl: IO[Unit] =
    primitiveBindings >>= (e => until_((x:String) => x == "quit")(readPrompt("lisp>> "))(evalAndPrint(e)))

  override def main(args: Array[String]) = {
    (if ( args.length == 0 ) runRepl else runOne(args)).unsafePerformIO()
  }
}
