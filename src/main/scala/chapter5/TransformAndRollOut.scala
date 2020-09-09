package chapter5

object TransformAndRollOut {

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  import scala.concurrent.Await

  import cats.data.EitherT
  import cats.instances.future._

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map("Jazz" -> 6, "Bumblebee" -> 8, "Hot Rod" -> 10)

  def getPowerLevel(autobot: String): Response[Int] = powerLevels.get(autobot) match {
    case Some(power) => EitherT.right(Future(power))
    case None => EitherT.left(Future(s"${autobot} unreachable"))
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield (power1 + power2 > 15)

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value
    Await.result(stack, 1.second) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }
  }

  def main(args: Array[String]): Unit = {
    println(tacticalReport("Jazz", "Bumblebee"))

    println(tacticalReport("Bumblebee", "Hot Rod"))

    println(tacticalReport("Jazz", "Ironhide"))
  }

}
