package chapter9



object MapReduce {

  import cats.Functor
  import cats.Monoid
  import cats.instances.int._
  import cats.instances.vector._
  import cats.instances.future._
  import cats.instances.string._

  import cats.syntax.semigroup._
  import cats.syntax.foldable._
  import cats.syntax.traverse._

  import scala.concurrent.Future
  import scala.concurrent.Await
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global


  def foldMap[A, B : Monoid](values: Vector[A])(f: A => B): B =
    //values.map(f).foldLeft(Monoid[B].empty)(Monoid[B].combine)
    values.foldLeft(Monoid[B].empty)(_ |+| f(_))

  def parallelFoldMap[A, B : Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * values.length / numCores).ceil.toInt
    val groups = values.grouped(groupSize)
    val futures =
      // groups.map(group => Future(group.map(f).foldLeft(Monoid[B].empty)(Monoid[B].combine)))
      groups.map(group => Future(group.foldLeft(Monoid[B].empty)(_ |+| f(_))))
    Future.sequence(futures).map(f => f.foldLeft(Monoid[B].empty)(Monoid[B].combine))
  }

  def parallelFoldMapWithCats[A, B : Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors()
    val groupSize = (1.0 * values.length / numCores).ceil.toInt
    values.grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(f)))
      .map(_.combineAll)
  }

  def main(args: Array[String]): Unit = {
    println(foldMap(Vector(1, 2, 3))(identity))
    println(foldMap(Vector(1, 2, 3))(_.toString + "! "))
    println(foldMap("Hello world!".toVector)(_.toString.toUpperCase))

    val result = parallelFoldMap((1 to 1000000).toVector)(identity)
    println(Await.result(result, 1.second))

    val future = parallelFoldMapWithCats((1 to 1000000).toVector)(identity)
    println(Await.result(future, 1.second))
  }

}
