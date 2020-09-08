package chapter3


object ContravariantInvariantInCats {

  import cats.Contravariant
  import cats.Monoid
  import cats.Show
  import cats.instances.string._
  import cats.syntax.contravariant._
  import cats.syntax.invariant._
  import cats.syntax.semigroup._
  import cats.syntax.show._

  implicit val symbolShow: Show[Symbol] = Show[String].contramap[Symbol](sym => s"'${sym.name}")
  // ignore the idea highlight error
  implicit val symbolMonoid: Monoid[Symbol] = Monoid[String].imap[Symbol](Symbol(_))(_.name)

  def main(args: Array[String]): Unit = {
    println(Symbol("x").show)
    println(Monoid[Symbol].empty)
    println(Symbol("a") |+| Symbol("few") |+| Symbol("words"))
  }

}
