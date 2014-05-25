package com.potix2.scheme

import scalaz._
import Scalaz._
import scalaz.effect._
import com.potix2.scheme.LispError._

trait LispEnv {
   type Env = IORef[List[(String, IORef[LispVal])]]

  def nullEnv: IO[Env] = IO.newIORef(List.empty[(String, IORef[LispVal])])

  implicit def liftIO = new MonadIO[IOThrowsError]{
    override def liftIO[A](ioa: IO[A]): LispError.IOThrowsError[A] = EitherT.right(ioa)
    override def point[A](a: => A): LispError.IOThrowsError[A] = EitherT.right(IO(a))
    override def bind[A, B](fa: LispError.IOThrowsError[A])(f: (A) => LispError.IOThrowsError[B]): LispError.IOThrowsError[B] = fa.flatMap(f)
  }

  def runIOThrows(action: IOThrowsError[String]): IO[String] = for {
    x <- action.run
  } yield x match {
      case \/-(v) => v
      case -\/(e) => e.toString()
    }

  def isBound(envRef: Env, varName: String): IO[Boolean] = for {
    e <- envRef.read
  } yield lookup(e, varName) match {
      case Some(_) => true
      case None => false
    }

  def lookup(env: List[(String, IORef[LispVal])], varName: String): Option[IORef[LispVal]] =
    env.find(x => x._1 == varName).map(_._2)

  def throwError[A](err: LispError): IOThrowsError[A] = EitherT.left(err.point[IO])

  def getVar(envRef: Env, varName: String): IOThrowsError[LispVal] = for {
      env <- envRef.read.liftIO[IOThrowsError]
      value <- lookup(env, varName) match {
        case Some(ioRef) => ioRef.read.liftIO[IOThrowsError]
        case None => throwError(UnboundVar("Getting an unbound variable: ", varName))
      }
    } yield value

  def setVar(envRef: Env, varName: String, value: LispVal): IOThrowsError[LispVal] = for {
    env <- envRef.read.liftIO[IOThrowsError]
    _ <- lookup(env, varName) match {
      case Some(ioRef) => ioRef.write(value).liftIO[IOThrowsError]
      case None => throwError(UnboundVar("Getting an unbound variable: ", varName))
    }
  } yield value

  def defineVar(envRef: Env, varName: String, value: LispVal): IOThrowsError[LispVal] = for {
    alreadyDefined <- isBound(envRef, varName).liftIO[IOThrowsError]
    v <- if (alreadyDefined)
      setVar(envRef, varName, value)
    else
      (for {
        valueRef <- IO.newIORef(value)
        env <- envRef.read
        _ <- envRef.write((varName, valueRef) :: env)
      } yield value).liftIO[IOThrowsError]
  } yield value

  def liftThrows[A](err: ThrowsError[A]): IOThrowsError[A] = err match {
    case -\/(e) => throwError(e)
    case \/-(v) => v.point[IOThrowsError]
  }

  /*
  def bindVars(envRef: Env, bindings: List[(String, LispVal)]): IO[Env] = {
    def addBinding(varName: String, value: LispVal) = for {
      ref <- IO.newIORef(value)
    } yield(varName, ref)

    def extendEnv(bindings: List[(String, LispVal)], env: Env) =
      env.map(bindings.map(x => addBinding(x._1, x._2)) ++ _)

    IO.newIORef(extendEnv(envRef.read, envRef))
  }
  */
}

