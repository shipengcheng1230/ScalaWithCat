package chapter1

object PrintableApp {

  final case class Cat(name: String, age: Int, color: String)

  trait Printable[A] {
    def format(value: A): String
  }

  object PrintableInstances {
    implicit val intPrintable: Printable[Int] =
      (value: Int) => value.toString

    implicit val stringPrintable: Printable[String] =
      (value: String) => value

    implicit val catPrintable: Printable[Cat] =
      (cat: Cat) =>
        s"${Printable.format(cat.name)} is a ${Printable.format(cat.age)} year-old ${Printable.format(cat.color)} cat."
  }

  object Printable {
    def format[A](input: A)(implicit p: Printable[A]): String = p.format(input)

    def print[A](input: A)(implicit p: Printable[A]): Unit = println(format(input))
  }

  object PrintableSyntax {
    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Printable[A]): String = p.format(value)

      def print(implicit p: Printable[A]): Unit = println(format(p))
    }
  }

  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  implicit val catShow: Show[Cat] = Show.show(cat => {
    s"${cat.name.show} is a ${cat.age.show} year-old ${cat.color.show} cat."
  })

  def main(args: Array[String]): Unit = {
    import PrintableInstances._
    import PrintableSyntax._

    val cat = Cat("Bob", 5, "yellow")
    cat.print
    println(cat.show)
  }

}
