package com.potix2.scheme
import scala.reflect.{ClassTag, classTag}

import com.potix2.scheme.Lisp._
import scalaz.Scalaz._
import scalaz.effect.IO
import java.io._
import scala.Some
import scalaz.\/-
import scalaz.-\/

object Port {
  def getReader(reader: => Reader): IO[BufferedReader] = IO(new BufferedReader(reader))
  def getWriter(writer: => Writer): IO[BufferedWriter] = IO(new BufferedWriter(writer))

  def closeReader(reader: BufferedReader):IO[Unit] = IO(reader.close())
  def closeWriter(writer: BufferedWriter):IO[Unit] = IO(writer.close())
}

trait Evaluator { self: LispEnv with LispParser =>

  val primitives = List(
    ("+", numericBinop (_ + _)),
    ("-", numericBinop (_ - _)),
    ("*", numericBinop (_ * _)),
    //("/", numericBinop (_ / _)),
    ("mod", numericBinop (_ % _)),

    ("=", numBoolBinop (_ == _)),
    ("<", numBoolBinop (_ < _)),
    (">", numBoolBinop (_ > _)),
    ("/=", numBoolBinop (_ != _)),
    (">=", numBoolBinop (_ >= _)),
    ("<=", numBoolBinop (_ <= _)),

    ("&&", boolBoolBinop (_ && _)),
    ("||", boolBoolBinop (_ || _)),

    ("string=?", strBoolBinop (_ == _)),
    ("string<?", strBoolBinop (_ < _)),
    ("string>?", strBoolBinop (_ > _)),
    ("string<=?", strBoolBinop (_ <= _)),
    ("string>=?", strBoolBinop (_ >= _)),

    // list primitives
    ("car", carExpr _),
    ("cdr", cdrExpr _),
    ("cons", consExpr _),

    // predicate for equality
    ("eq?", eqvExpr _),
    ("eqv?", eqvExpr _),
    ("equal?", equalExpr _),

    // type primitives
    ("boolean?", typeTestOp[LispBool]),
    ("string?", typeTestOp[LispString]),
    ("symbol?", typeTestOp[LispAtom]),
    ("number?", typeTestOp[LispInteger])
  )

  val ioPrimitives = List(
    ("apply", applyProc _),
    ("open-input-file", makeReaderPort _),
    ("open-output-file", makeWriterPort _),
    ("close-input-port", closePort _),
    ("close-output-port", closePort _),
    ("read", readProc _),
    ("write", writeProc _),
    ("read-contents", readContents _),
    ("read-all", readAll _)
  )

  def typeTestOp[A <: LispVal: ClassTag]: List[LispVal] => ThrowsError[LispVal] = {
    case xs@(h :: Nil) => LispBool(classTag[A].runtimeClass.isInstance(h)).point[ThrowsError]
    case xs => NumArgs(1,xs).left[LispVal]
  }

  def numericBinop(op: (Int, Int) => Int): List[LispVal] => ThrowsError[LispVal] = {
    case xs@(h :: Nil) => NumArgs(2, xs).left[LispVal]
    case xs => for {
      ints <- xs.map(unpackNumber).sequence[ThrowsError, Int]
    } yield LispInteger(ints.tail.foldLeft(ints.head)(op))
  }

  def numBoolBinop = boolBinop(unpackNumber)(_)
  def strBoolBinop = boolBinop(unpackString)(_)
  def boolBoolBinop = boolBinop(unpackBool)(_)

  def boolBinop[A](unpacker: LispVal => ThrowsError[A])(op: (A,A) => Boolean): List[LispVal] => ThrowsError[LispVal] = {
    case xs@(lhs :: rhs :: Nil) => for {
      l <- unpacker(lhs)
      r <- unpacker(rhs)
    } yield LispBool(op(l, r))
    case xs => NumArgs(2, xs).left[LispVal]
  }

  def unpackNumber(value: LispVal): ThrowsError[Int] = value match {
    case LispInteger(n) => n.point[ThrowsError]
    case v => TypeMismatch("number", v).left[Int]
  }

  def unpackString(value: LispVal): ThrowsError[String] = value match {
    case LispString(v) => v.point[ThrowsError]
    case LispInteger(v) => v.toString.point[ThrowsError]
    case LispBool(v) => v.toString.point[ThrowsError]
    case v => TypeMismatch("string", v).left[String]
  }

  def unpackBool(value: LispVal): ThrowsError[Boolean] = value match {
    case LispBool(b) => b.point[ThrowsError]
    case v => TypeMismatch("boolean", v).left[Boolean]
  }

  def carExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case xs@(h::Nil) => h match {
      case xs@(LispList(y :: ys)) => y.point[ThrowsError]
      case xs@(LispDottedList(y :: ys, value)) => y.point[ThrowsError]
      case badArg => TypeMismatch("pair", badArg).left[LispVal]
    }
    case xs@(h::t) => NumArgs(1, xs).left[LispVal]
  }

  def cdrExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case xs@(h::Nil) => h match {
      case xs@(LispList(y :: ys)) => LispList(ys).point[ThrowsError]
      case xs@(LispDottedList(y :: Nil, value)) => value.point[ThrowsError]
      case xs@(LispDottedList(y :: ys, value)) => LispDottedList(ys, value).point[ThrowsError]
      case badArg => TypeMismatch("pair", badArg).left[LispVal]
    }
    case xs@(h::t) => NumArgs(1, xs).left[LispVal]
  }

  def consExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case h::Nil                            => LispList(List(h)).point[ThrowsError]
    case x::LispDottedList(ys,yslast)::Nil => LispDottedList(x::ys, yslast).point[ThrowsError]
    case h::LispList(ys)::Nil              => LispList(h::ys).point[ThrowsError]
    case h::t::Nil                         => LispDottedList(List(h), t).point[ThrowsError]
    case xs@(h::t) => NumArgs(2, xs).left[LispVal]
  }

  def eqvExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case LispBool(b1)::LispBool(b2)::Nil => LispBool(b1 == b2).point[ThrowsError]
    case LispInteger(n1)::LispInteger(n2)::Nil => LispBool(n1 == n2).point[ThrowsError]
    case LispString(s1)::LispString(s2)::Nil => LispBool(s1 == s2).point[ThrowsError]
    case LispAtom(a1)::LispAtom(a2)::Nil => LispBool(a1 == a2).point[ThrowsError]
    case LispDottedList(xs1, x1)::LispDottedList(xs2,x2)::Nil => eqvExpr(List(LispList(xs1 ++ List(x1)), LispList(xs2 ++ List(x2))))
    case LispList(xs1)::LispList(xs2)::Nil =>
      LispBool(xs1.length == xs2.length && (xs1 zip xs2 forall { s =>
        eqvExpr(List(s._1,s._2)) match {
          case -\/(err) => false
          case \/-(LispBool(v)) => v
        }})).point[ThrowsError]
    case _::_::Nil => LispBool(false).point[ThrowsError]
    case badArgList => NumArgs(2, badArgList).left[LispVal]
  }

  case class AnyUnpacker[A](unpacker: LispVal => ThrowsError[A])
  val unpackers = List(AnyUnpacker(unpackNumber), AnyUnpacker(unpackString), AnyUnpacker(unpackBool))
  def unpackEquals[A](arg1: LispVal, arg2: LispVal)(unpacker: AnyUnpacker[A]): ThrowsError[Boolean] = {
    val b = for {
      unpacked1 <- unpacker.unpacker(arg1)
      unpacked2 <- unpacker.unpacker(arg2)
    } yield unpacked1 == unpacked2
    if (b.isLeft) false.point[ThrowsError] else b
  }

  def equalExpr(args: List[LispVal]): ThrowsError[LispVal] = args match {
    case LispDottedList(xs1, x1)::LispDottedList(xs2,x2)::Nil => equalExpr(List(LispList(xs1 ++ List(x1)), LispList(xs2 ++ List(x2))))
    case LispList(xs1)::LispList(xs2)::Nil => LispBool(xs1 zip xs2 forall { s =>
      equalExpr(List(s._1,s._2)) match {
        case -\/(err) => false
        case \/-(LispBool(v)) => v
      }}).point[ThrowsError]
    case arg1::arg2::Nil => for {
      primitiveEquals <- unpackers.map(unpackEquals(arg1, arg2)(_)).sequence[ThrowsError, Boolean].map(x => x.foldLeft(false)(_ || _))
      eqvEquals <- eqvExpr(args)
    } yield {
      val eqvEqualsInner = eqvEquals match {
        case LispBool(v) => v
        case _ => false
      }
      LispBool(primitiveEquals || eqvEqualsInner)
    }
    case badArgList => NumArgs(2, badArgList).left[LispVal]
  }

  def exprCond(env: Env, clauses: List[LispVal]): IOThrowsError[LispVal] = clauses.toStream.map {
    case LispList(LispAtom("else")::body) => exprCondImpl(env, LispBool(true), body)
    case LispList(test::LispAtom("=>")::expr::Nil) => exprCondImpl(env, test, List(LispList(List(expr, test))))
    case LispList(test::body) => exprCondImpl(env, test, body)
  }.sequence[IOThrowsError, Option[LispVal]].map { _.flatten.head }

  def exprCondImpl(env: Env, test:LispVal, body:List[LispVal]): IOThrowsError[Option[LispVal]] = for {
    testResult <- eval(env)(test)
    result <- if (testResult == LispBool(false)) None.point[IOThrowsError] else body.map(eval(env)).sequence[IOThrowsError, LispVal].map(x => x.lastOption.getOrElse(testResult).some)
  } yield result

  def _apply(func: LispVal, args: List[LispVal]): IOThrowsError[LispVal] = func match {
    case LispPrimitiveFunc(func) => liftThrows(func(args))
    case LispFunc(params, varargs, body, closure) => {
      def remainingArgs = args.drop(params.length)
      def evalBody(env: Env): IOThrowsError[LispVal] = body.map(eval(env)).sequence[IOThrowsError, LispVal].map(_.last)
      def bindVarArgs(arg: Option[String])(env: Env) = arg match {
        case Some(argName) => bindVars(env)(List((argName, LispList(remainingArgs)))).liftIO[IOThrowsError]
        case None => env.point[IOThrowsError]
      }

      if(params.length != args.length && !varargs.isDefined)
        throwError(NumArgs(params.length, args))
      else
        for {
          e1 <- bindVars(closure)(params.zip(args)).liftIO[IOThrowsError]
          e2 <- bindVarArgs(varargs)(e1)
          e3 <- evalBody(e2)
        } yield e3
    }
    case LispIOFunc(func) => func(args)
    case _ => throwError(NotFunction("not function", func.toString))
  }

  def makeFunc(varargs: Option[String])(envRef: Env)(params: List[LispVal])(body: List[LispVal]):IOThrowsError[LispVal] =
    LispFunc(params.map(_.toString), varargs, body, envRef).point[IOThrowsError]

  def makeNormalFunc(envRef: Env)(params: List[LispVal])(body: List[LispVal]) =
    makeFunc(None)(envRef)(params)(body)

  def makeVarargs(value: LispVal)(envRef: Env)(params: List[LispVal])(body: List[LispVal]) =
    makeFunc(Some(value.toString))(envRef)(params)(body)

  def makeReaderPort(values: List[LispVal]): IOThrowsError[LispVal] = values match {
    case LispString(fileName) :: _ => Port.getReader(new FileReader(fileName)).liftIO[IOThrowsError].map(LispReaderPort(_))
  }

  def makeWriterPort(values: List[LispVal]): IOThrowsError[LispVal] = values match {
    case LispString(fileName) :: _ => Port.getWriter(new FileWriter(fileName)).liftIO[IOThrowsError].map(LispWriterPort(_))
  }

  def closePort(values: List[LispVal]): IOThrowsError[LispVal] = (values match {
    case LispReaderPort(reader) :: Nil => { reader.close(); LispBool(true) }
    case LispWriterPort(writer) :: Nil => { writer.close(); LispBool(true) }
    case _ => LispBool(false)
  }).point[IOThrowsError]

  def readProc(values: List[LispVal]): IOThrowsError[LispVal] = values match {
    case Nil => for {
      port <- Port.getReader(new InputStreamReader(System.in)).map(LispReaderPort(_)).liftIO[IOThrowsError]
      result <- readProc(List(port))
    } yield result
    case LispReaderPort(reader) :: Nil => for {
      input <- IO(reader.readLine()).liftIO[IOThrowsError]
      result <- liftThrows(readExpr(input))
    } yield result
  }

  def writeProc(values: List[LispVal]): IOThrowsError[LispVal] = values match {
    case v :: Nil => for {
      port <- Port.getWriter(new OutputStreamWriter(System.out)).map(LispWriterPort(_)).liftIO[IOThrowsError]
      result <- writeProc(List(v, port))
    } yield result
    case v :: LispWriterPort(writer) :: Nil => for {
      _ <- IO(writer.write(v.toString)).liftIO[IOThrowsError]
    } yield LispBool(true)
  }

  def readContents(values: List[LispVal]): IOThrowsError[LispVal] = values match {
    case LispString(fileName) :: Nil => IO(LispString(scala.io.Source.fromFile(fileName).mkString)).liftIO[IOThrowsError]
  }

  def load(fileName: String): IOThrowsError[List[LispVal]] = for {
    contents <- IO(scala.io.Source.fromFile(fileName).mkString).liftIO[IOThrowsError]
    result <- liftThrows(readExprList(contents))
  } yield result

  def readAll(values: List[LispVal]): IOThrowsError[LispVal] = values match {
    case LispString(fileName) :: Nil => load(fileName).map(LispList(_))
  }

  def applyProc(values: List[LispVal]): IOThrowsError[LispVal] = values match {
    case func :: LispList(args) :: Nil => _apply(func, args)
    case func :: args => _apply(func, args)
  }

  def primitiveBindings: IO[Env] = {
    nullEnv >>= FuncUtil.flip(bindVars)(ioPrimitives.map(y => (y._1, LispIOFunc(y._2)))
      ++ primitives.map(y => (y._1, LispPrimitiveFunc(y._2))))
  }

  def ifExpr(env: Env, pred: LispVal, coseq: LispVal, alt: LispVal): IOThrowsError[LispVal] = for {
    cond <- eval(env)(pred)
    result <- cond match {
      case LispBool(false) => eval(env)(alt)
      case _ => eval(env)(coseq)
    }
  } yield result

  def eval(env: Env)(value: LispVal): IOThrowsError[LispVal] = value match {
    case LispList(List(LispAtom("quote"), v)) => v.point[IOThrowsError]
    case LispList(List(LispAtom("if"), pred, coseq, alt)) => ifExpr(env, pred, coseq, alt)
    case LispList(LispAtom("cond") :: clauses) => exprCond(env, clauses)
    case LispList(LispAtom("define") :: LispList(LispAtom(varName) :: params) :: body) =>
      for {
        f <- makeNormalFunc(env)(params)(body)
        result <- defineVar(env)(varName)(f)
      } yield result
    case LispList(LispAtom("define") :: LispDottedList(LispAtom(varName) :: params, varargs) :: body) =>
      for {
        f <- makeVarargs(varargs)(env)(params)(body)
        result <- defineVar(env)(varName)(f)
      } yield result
    case LispList(LispAtom("define") :: LispAtom(varName) :: form :: Nil) => eval(env)(form) >>= defineVar(env)(varName)
    case LispList(LispAtom("lambda") :: LispList(params) :: body) => makeNormalFunc(env)(params)(body)
    case LispList(LispAtom("lambda") :: LispDottedList(params, varargs) :: body) => makeVarargs(varargs)(env)(params)(body)
    case LispList(LispAtom("lambda") :: LispAtom(x) :: body) => makeVarargs(LispAtom(x))(env)(List())(body)
    case LispList(LispAtom("set!") :: LispAtom(v) :: form :: Nil) => eval(env)(form) >>= setVar(env)(v)
    case LispList(LispAtom("load") :: LispString(fileName) :: Nil) => load(fileName) >>= (x => x.map(eval(env)(_)).sequence[IOThrowsError, LispVal].map(_.last))
    case LispList(func :: args) => for {
      f <- eval(env)(func)
      argVals <- args.map(eval(env)).sequence[IOThrowsError, LispVal]
      result <- _apply(f, argVals)
    } yield result

    case s:LispString => s.point[IOThrowsError]
    case n:LispNumber => n.point[IOThrowsError]
    case b:LispBool   => b.point[IOThrowsError]
    case LispAtom(id) => getVar(env, id)
    case LispList(Nil) => value.point[IOThrowsError]
    case v => throwError(BadSpecialForm("Unrecognized special form", v))
  }
}
