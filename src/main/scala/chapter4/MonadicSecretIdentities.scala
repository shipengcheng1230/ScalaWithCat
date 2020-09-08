package chapter4

object MonadicSecretIdentities {

  import cats.Id

  def pure[A](value: A): Id[A] = value
  // type Id[A] = A
  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)

  def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)

  def main(args: Array[String]): Unit = {
    println(map(123)(_ * 2))
    println(flatMap(123)(_ + 2))
  }

}
