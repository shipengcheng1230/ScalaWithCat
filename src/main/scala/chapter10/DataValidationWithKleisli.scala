package chapter10

object DataValidationWithKleisli {

  import cats.data.{Kleisli, NonEmptyList}
  import cats.Semigroup
  import cats.data.Validated
  import cats.data.Validated.{Invalid, Valid}
  import cats.instances.list._
  import cats.syntax.semigroup._
  import cats.syntax.either._
  import cats.syntax.validated._
  import cats.syntax.apply._


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
    // use Kleisli
    def run(implicit s: Semigroup[E]): A => Either[E, A] = a => this(a).toEither
  }

  object Predicate {

    final case class And[E, A](left: Predicate[E, A], that: Predicate[E, A]) extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], that: Predicate[E, A]) extends Predicate[E, A]

    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

    def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] = Pure(a => if (fn(a)) a.valid else err.invalid)
  }

  def main(args: Array[String]): Unit = {

    type Errors = NonEmptyList[String]

    type Result[A] = Either[Errors, A]

    type Check[A, B] = Kleisli[Result, A, B]

    def check[A, B](func: A => Result[B]): Check[A, B] = Kleisli(func)

    def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] = Kleisli(pred.run)

    def error(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)

    def longerThan(n: Int): Predicate[Errors, String] = Predicate.lift(
      error(s"Must be longer than $n characters"), str => str.length > n)

    val alphanumeric: Predicate[Errors, String] = Predicate.lift(
      error(s"Must be all alphanumeric characters"), str => str.forall(_.isLetterOrDigit))

    def contains(char: Char): Predicate[Errors, String] = Predicate.lift(
      error(s"Must contain the character $char"), str => str.contains(char))

    def containsOnce(char: Char): Predicate[Errors, String] = Predicate.lift(
      error(s"Must contain the character $char only once"), str => str.count(c => c == char) == 1)

    val checkUsername: Check[String, String] = checkPred(longerThan(3) and alphanumeric)

    val splitEmail: Check[String, (String, String)] = check(str => str.split('@') match {
      case Array(name, domain) => Right((name, domain))
      case _ => Left(error("Must contain a single @ character"))
    })

    val checkLeft: Check[String, String] = checkPred(longerThan(0))

    val checkRight: Check[String, String] = checkPred(longerThan(3) and contains('.'))

    val joinEmail: Check[(String, String), String] = check {
      case (l, r) =>
        (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

    val checkEmail: Check[String, String] = splitEmail andThen joinEmail

    final case class User(username: String, email: String)

    def createUser(username: String, email: String): Either[Errors, User] =
      (checkUsername.run(username), checkEmail.run(email)).mapN(User)

    println(createUser("Noel", "noel@underscore.io"))

    println(createUser("", "dave@underscore.io@io"))
  }
}
