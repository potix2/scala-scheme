package com.potix2.scheme

import scalaz._
import Scalaz._
import scalaz.effect.IO

object MyLisp extends App with LispParser with Evaluator {

  def readPrompt(prompt: String): IO[String] = for {
    _ <- IO.putStr(prompt)
    line <- IO.readLn
  } yield line

  def evalString(expr: String): IO[String] = {
    val extractValue = for {
      e <- readExpr(expr)
      evaled <- eval(e)
    } yield evaled

    (extractValue match {
      case -\/(e) => e.toString()
      case \/-(v) => v.toString
    }).point[IO]
  }

  def evalAndPrint(expr: String): IO[Unit] = for {
    p <- evalString(expr)
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

  def runRepl:IO[Unit] =
    until_((x:String) => x == "quit")(readPrompt("lisp>> "))(evalAndPrint)

  override def main(args: Array[String]) = {
    (if ( args.length == 0 )
      runRepl
    else
      evalAndPrint(args(0))).unsafePerformIO()
  }
}
