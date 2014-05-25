package com.potix2.scheme

import scalaz._
import Scalaz._
import scalaz.effect.{IORef, IO}

object MyLisp extends App with LispParser with Evaluator with LispEnv {
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

  def runOne(expr: String): IO[Unit] = for {
    env <- nullEnv
    _ <- evalAndPrint(env)(expr)
  } yield()

  def runRepl: IO[Unit] = for {
    env <- nullEnv
    _ <- until_((x:String) => x == "quit")(readPrompt("lisp>> "))(evalAndPrint(env))
  } yield()

  override def main(args: Array[String]) = {
    if ( args.length == 0 )
      runRepl.unsafePerformIO()
    else
      runOne(args(0)).unsafePerformIO()
  }
}
