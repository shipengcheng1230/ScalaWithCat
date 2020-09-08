package chapter3

object ShowingOffWithContramap {

  trait Printable[A] { self =>
    def format(value: A): String
    // prepend operation
    def contramap[B](func: B => A): Printable[B] =
      (value: B) => self.format(func(value))
  }

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  implicit val stringPrintable: Printable[String] =
    (value: String) => s"'${value}'"

  implicit val booleanPrintable: Printable[Boolean] =
    (value: Boolean) => if (value) "yes" else "no"

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap(_.value)

  final case class Box[A](value: A)

  def main(args: Array[String]): Unit = {
    println(format("hello"))
    println(format(true))
    println(format(Box("hello")))
    println(format(Box(false)))
  }

}
