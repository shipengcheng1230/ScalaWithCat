package chapter2

object AddingAllTheThings {

  import cats.Monoid
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.semigroup._

  case class Order(totalCost: Double, quantity: Double)

  def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
    items.foldLeft(monoid.empty)(_ |+| _)

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
    override def empty: Order = Order(0, 0)
  }

  def main(args: Array[String]): Unit = {
    println(add(List(1, 2, 3)))
    println(add(List(Some(1), None, Some(2), None, Some(3))))
    // add(List(Some(1), Some(2), Some(3))) // not yet!
    println(add(List(Order(1, 2), Order(2, 3))))
  }

}
