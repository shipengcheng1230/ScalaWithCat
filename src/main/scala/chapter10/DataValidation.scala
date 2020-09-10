package chapter10

import scala.collection.View.FlatMap

object DataValidation {

  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated._
  import cats.instances.list._
  import cats.syntax.semigroup._
  import cats.syntax.either._
  import cats.syntax.validated._
  import cats.syntax.apply._


  // from a user input to a validation result
  final case class CheckF[E: Semigroup, A](func: A => Either[E, A]) {
    def apply(a: A): Either[E, A] = func(a)

    def and(that: CheckF[E, A]): CheckF[E, A] =
      CheckF { a =>
        (this (a), that(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e), Right(_)) => e.asLeft
          case (Right(_), Left(e)) => e.asLeft
          case (Right(_), Right(_)) => a.asRight
        }
      }
  }

  sealed trait Predicate[E, A] {

    import Predicate._

    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) => func(a)
        case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
        case Or(left, right) =>
          left(a) match {
            case Valid(_) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(_) => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  object Predicate {

    final case class And[E, A](left: Predicate[E, A], that: Predicate[E, A]) extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], that: Predicate[E, A]) extends Predicate[E, A]

    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

    def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] = Pure(a => if (fn(a)) a.valid else err.invalid)
  }

  sealed trait Check[E, A, B] {

    import Check._

    def apply(a: A): Validated[E, B]

    def map[C](func: B => C): Check[E, A, C] = Map(this, func)

    def flatMap[C](func: B => Check[E, A, C]): Check[E, A, C] = FlatMap(this, func)

    def andThen[C](next: Check[E, B, C]): Check[E, A, C] = AndThen(this, next)
  }

  object Check {

    final case class Map[E, A, B, C](check: Check[E, A, B], func: B => C) extends Check[E, A, C] {
      override def apply(a: A): Validated[E, C] = check(a).map(func)
    }

    final case class FlatMap[E, A, B, C](check: Check[E, A, B], func: B => Check[E, A, C]) extends Check[E, A, C] {
      override def apply(a: A): Validated[E, C] =
        check(a).withEither(_.flatMap(b => func(b)(a).toEither))
    }

    final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
      override def apply(a: A): Validated[E, C] =
        check1(a).withEither(_.flatMap(b => check2(b).toEither))
    }

    final case class PurePredicate[E: Semigroup, A](pred: Predicate[E, A]) extends Check[E, A, A] {
      override def apply(a: A): Validated[E, A] = pred(a)
    }

    final case class Pure[E, A, B](func: A => Validated[E, B]) extends Check[E, A, B] {
      override def apply(a: A): Validated[E, B] = func(a)
    }

    def apply[E: Semigroup, A](pred: Predicate[E, A]): Check[E, A, A] = PurePredicate(pred)

    def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] = Pure(func)
  }

  def checkEmailExample(): Unit = {

    import cats.data.{NonEmptyList, Validated}

    type Errors = NonEmptyList[String]

    def error(s: String): NonEmptyList[String] =
      NonEmptyList(s, Nil)

    def longerThan(n: Int): Predicate[Errors, String] = Predicate.lift(
      error(s"Must be longer than $n characters"), str => str.length > n)

    val alphanumeric: Predicate[Errors, String] = Predicate.lift(
      error(s"Must be all alphanumeric characters"), str => str.forall(_.isLetterOrDigit))

    def contains(char: Char): Predicate[Errors, String] = Predicate.lift(
      error(s"Must contain the character $char"), str => str.contains(char))

    def containsOnce(char: Char): Predicate[Errors, String] = Predicate.lift(
      error(s"Must contain the character $char only once"), str => str.count(c => c == char) == 1)

    val checkUsername = Check(longerThan(3) and alphanumeric)

    val splitEmail = Check[Errors, String, (String, String)](str => str.split('@') match {
      case Array(name, domain) => (name, domain).validNel
      case _ => "Must contain a single @ character".invalidNel
    })

    val checkLeft = Check(longerThan(0))

    val checkRight = Check(longerThan(3) and contains('.'))

    val joinEmail = Check[Errors, (String, String), String] {
      case (l, r) =>
        (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

    val checkEmail: Check[Errors, String, String] = splitEmail andThen joinEmail

    final case class User(username: String, email: String)

    def createUser(username: String, email: String): Validated[Errors, User] =
      (checkUsername(username), checkEmail(email)).mapN(User)

    println(createUser("Noel", "noel@underscore.io"))

    println(createUser("", "dave@underscore.io@io"))
  }

  def main(args: Array[String]): Unit = {
    val a = CheckF[List[String], Int] { v => if (v > 2) v.asRight else List("Must be > 2").asLeft }
    val b = CheckF[List[String], Int] { v => if (v < -2) v.asRight else List("Must be < -2").asLeft }
    val check1 = a and b
    println(check1(5), check1(0))

    val c = Predicate[List[String], Int] { v => if (v > 3) v.valid else List("Must be > 3").invalid }
    val d = Predicate[List[String], Int] { v => if (v < -3) v.valid else List("Must be < -3").invalid }
    val check2 = c and d
    println(check2(5), check2(0))

    val check3 = c or d
    println(check3(5), check3(0))

    checkEmailExample()
  }

}
