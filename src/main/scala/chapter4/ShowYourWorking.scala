package chapter4

object ShowYourWorking {

  def slowly[A](body: => A): A = try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  import cats.data.Writer
  import cats.instances.vector._
  import cats.syntax.applicative._
  import cats.syntax.writer._

  type Logged[A] = Writer[Vector[String], A]

  def factorialWithWriter(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) 1.pure[Logged] else {
        slowly(factorialWithWriter(n - 1).map(_ * n))
      }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }

  def main(args: Array[String]): Unit = {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits._
    import scala.concurrent.duration._

    Await.result(
      Future.sequence(Vector(Future(factorial(5)), Future(factorial(5)))),
      5.seconds
    )

    println(factorialWithWriter(5).run)

    println(Await.result(
      Future.sequence(Vector(Future(factorialWithWriter(5)), Future(factorialWithWriter(5))).map(_.map(_.written))),
      5.seconds
    ))
  }

}
