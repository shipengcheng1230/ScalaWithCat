package chapter6

object ProductOfList {

  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import cats.syntax.applicative._

  import cats.Monad
  import cats.syntax.parallel._

  def product[F[_]: Monad, A, B](x: F[A], y: F[B]): F[(A, B)] =
    x.flatMap(a => y.map(b => (a, b)))

  import cats.instances.list._

  def main(args: Array[String]): Unit = {
    println(product(List(1, 2), List(3, 4)))
    println((List(1, 2), List(3, 4)).parTupled)
  }

}
