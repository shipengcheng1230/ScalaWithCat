package chapter1

object MeetCats {

  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  import java.util.Date

  implicit val dateShow: Show[Date] =
    Show.show(date => s"${date.getTime} ms since the epoch.")

  def main(args: Array[String]): Unit = {
    println(new Date().show)
  }
}
