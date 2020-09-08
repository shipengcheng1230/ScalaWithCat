package chapter4

object BranchingOutFurther {

  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  def leaf[A](value: A): Tree[A] = Leaf(value)

  import cats.Monad
  import scala.annotation.tailrec

  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      case Leaf(x) => f(x)
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      // non tailrec
      flatMap(f(a)) {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => Leaf(value)
      }
    }

    // https://stackoverflow.com/a/51183572/8697614
    def tailRecM_[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {

      @tailrec
      def loop(toVisit: List[Tree[Either[A, B]]],
               toCollect: List[Option[Tree[B]]]): List[Tree[B]] =
        toVisit match {
          case Branch(l, r) :: next =>
            loop(l :: r :: next, None :: toCollect)

          case Leaf(Left(value)) :: next =>
            loop(f(value) :: next, toCollect)

          case Leaf(Right(value)) :: next =>
            loop(next, Some(pure(value)) :: toCollect)

          case Nil =>
            toCollect.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
              maybeTree.map(_ :: acc).getOrElse {
                val left :: right :: tail = acc
                branch(left, right) :: tail
              }
            }
        }

      loop(List(f(a)), Nil).head
    }
  }

  def main(args: Array[String]): Unit = {

    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatMap

    branch(leaf(100), leaf(200)).
      flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

    for {
      a <- branch(leaf(100), leaf(200))
      b <- branch(leaf(a - 10), leaf(a + 10))
      c <- branch(leaf(b - 1), leaf(b + 1))
    } yield c

  }

}
