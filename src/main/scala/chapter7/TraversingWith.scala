package chapter7


object TraversingWith {

  import cats.Applicative
  import cats.syntax.applicative._
  import cats.syntax.apply._
  import cats.instances.option._
  import cats.instances.list._
  import cats.data.Validated

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) => (accum, func(item)).mapN(_ :+ _) }

  def listSequence[F[_] : Applicative, B](list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  def process(inputs: List[Int]): Option[List[Int]] =
    listTraverse(inputs)(n => if(n % 2 == 0) Some(n) else None)

  type ErrorsOr[A] = Validated[List[String], A]

  def process2(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) Validated.valid(n)
      else Validated.invalid(List(s"$n is not even"))
    }

  def main(args: Array[String]): Unit = {
    println(listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6))))
    println(process(List(0, 2, 6)))
    println(process(List(0, 3, 6)))
    println(process2(List(0, 3, 7)))
  }

}
