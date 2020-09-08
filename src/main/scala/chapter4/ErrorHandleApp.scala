package chapter4

object ErrorHandleApp {

  import cats.MonadError
  import cats.instances.either._
  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  import cats.syntax.monadError._
  // import cats.implicits._

  import scala.util.Try

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    if (age >= 18) age.pure[F]
    else new IllegalArgumentException("Age must be greater than or equal\nto 18").raiseError[F, Int]

  def main(args: Array[String]): Unit = {
    println(validateAdult[Try](18))

    println(validateAdult[Try](8))

    type ExceptionOr[A] = Either[Throwable, A]

    println(validateAdult[ExceptionOr](-1))

    println(validateAdult[ExceptionOr](20))

  }

}
