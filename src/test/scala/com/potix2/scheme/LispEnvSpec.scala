package com.potix2.scheme

import org.specs2.mutable.Specification
import com.potix2.scheme.LispError.IOThrowsError
import com.potix2.scheme.LispEnv._

class LispEnvSpec extends Specification {
  val env = for {
    xx <- LispEnv.nullEnv
    _ <- xx.write(List(("a", LispInteger(1).toIORef), ("b", LispInteger(2).toIORef)))
  } yield xx
  "isBound" should {
    "be false when the environment is empty" in {
      LispEnv.isBound(LispEnv.nullEnv.unsafePerformIO(), "a").unsafePerformIO() must beFalse
    }
    "be false when the passed var doesn't exist" in {
      LispEnv.isBound(env.unsafePerformIO(), "c").unsafePerformIO() must beFalse
    }
    "be true when the passed var exist" in {
      LispEnv.isBound(env.unsafePerformIO(), "a").unsafePerformIO() must beTrue
    }
  }
  "getVar" should {
    "throw UnboundVar error when the passed var doesn't exist" in {
      LispEnv.getVar(LispEnv.nullEnv.unsafePerformIO(), "a").toEither.unsafePerformIO() must beLeft(beAnInstanceOf[UnboundVar])
    }
    "return LispVal when the passed var exists in an environment" in {
      LispEnv.getVar(env.unsafePerformIO(), "a").toEither.unsafePerformIO() must beRight(LispInteger(1))
    }
  }
  "setVar" should {
    "throw UnboundVar error when the passed var doesn't exist" in {
      LispEnv.setVar(LispEnv.nullEnv.unsafePerformIO(), "a", LispInteger(1)).toEither.unsafePerformIO() must beLeft(beAnInstanceOf[UnboundVar])
    }
    "return LispVal when the passed var exists in an environment" in {
      (for {
        e <- env.liftIO[IOThrowsError]
        _ <- LispEnv.setVar(e, "a", LispInteger(2))
        v <- LispEnv.getVar(e, "a")
      } yield v).toEither.unsafePerformIO() must beRight(LispInteger(2))
    }
  }
  "defineVar" should {
    "bind new value when the passed var doesn't exist" in {
      (for {
        e <- LispEnv.nullEnv.liftIO[IOThrowsError]
        _ <- LispEnv.defineVar(e, "a", LispInteger(100))
        v <- LispEnv.getVar(e, "a")
      } yield v).toEither.unsafePerformIO() must beRight(LispInteger(100))
    }
    "set new value when the passed var exists" in {
      (for {
        e <- env.liftIO[IOThrowsError]
        _ <- LispEnv.defineVar(e, "a", LispInteger(200))
        v <- LispEnv.getVar(e, "a")
      } yield v).toEither.unsafePerformIO() must beRight(LispInteger(200))
    }
  }
}
