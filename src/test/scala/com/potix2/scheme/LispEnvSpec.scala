package com.potix2.scheme

import org.specs2.mutable.Specification
import Lisp._
import scalaz.effect.IO

class LispEnvSpec extends Specification with LispEnv {

  val env = for {
    xx <- nullEnv
    _ <- xx.write(List(("a", LispInteger(1).toIORef), ("b", LispInteger(2).toIORef)))
  } yield xx
  "isBound" should {
    "be false when the environment is empty" in {
      isBound(nullEnv.unsafePerformIO(), "a").unsafePerformIO() must beFalse
    }
    "be false when the passed var doesn't exist" in {
      isBound(env.unsafePerformIO(), "c").unsafePerformIO() must beFalse
    }
    "be true when the passed var exist" in {
      isBound(env.unsafePerformIO(), "a").unsafePerformIO() must beTrue
    }
  }
  "getVar" should {
    "throw UnboundVar error when the passed var doesn't exist" in {
      getVar(nullEnv.unsafePerformIO(), "a").toEither.unsafePerformIO() must beLeft(beAnInstanceOf[UnboundVar])
    }
    "return LispVal when the passed var exists in an environment" in {
      getVar(env.unsafePerformIO(), "a").toEither.unsafePerformIO() must beRight(LispInteger(1))
    }
  }
  "setVar" should {
    "throw UnboundVar error when the passed var doesn't exist" in {
      setVar(nullEnv.unsafePerformIO(), "a", LispInteger(1)).toEither.unsafePerformIO() must beLeft(beAnInstanceOf[UnboundVar])
    }
    "return LispVal when the passed var exists in an environment" in {
      (for {
        e <- env.liftIO[IOThrowsError]
        _ <- setVar(e, "a", LispInteger(2))
        v <- getVar(e, "a")
      } yield v).toEither.unsafePerformIO() must beRight(LispInteger(2))
    }
  }
  "defineVar" should {
    "bind new value when the passed var doesn't exist" in {
      (for {
        e <- nullEnv.liftIO[IOThrowsError]
        _ <- defineVar(e, "a", LispInteger(100))
        v <- getVar(e, "a")
      } yield v).toEither.unsafePerformIO() must beRight(LispInteger(100))
    }
    "set new value when the passed var exists" in {
      (for {
        e <- env.liftIO[IOThrowsError]
        _ <- defineVar(e, "a", LispInteger(200))
        v <- getVar(e, "a")
      } yield v).toEither.unsafePerformIO() must beRight(LispInteger(200))
    }
  }

  "bindVars" should {
    "concatenate new bindings to passed environment" in {
      val e = bindVars(env.unsafePerformIO(), List(("a", LispInteger(1)), ("b", LispInteger(2)))).unsafePerformIO()
      getVar(e, "b").toEither.unsafePerformIO() must beRight(LispInteger(2))
      getVar(e, "a").toEither.unsafePerformIO() must beRight(LispInteger(1))
    }
  }
}
