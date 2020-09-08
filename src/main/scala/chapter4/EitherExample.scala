package chapter4

object EitherExample extends App {

  import cats.syntax.either._
  println(3.asRight[String])
  println("1".asRight[Int])
  println("baz".asRight[Int])
  println("bar".asLeft[Int])
  println("foo".asLeft[Int])

  println("error".asRight[Int].recoverWith {
    case _ => Right(-1)
  })
  println("error".asLeft[Int].recoverWith {
    case _ => Right(-1)
  })


}
