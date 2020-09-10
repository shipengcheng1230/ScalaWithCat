package chapter11

object CRDTs {

  import cats.kernel.CommutativeMonoid
  import cats.syntax.semigroup._
  import cats.syntax.foldable._
  import cats.instances.map._
  import cats.instances.list._

  trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
    def combine(a1: A, a2: A): A

    def empty: A
  }

  object BoundedSemiLattice {
    implicit val intInstance: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      override def combine(a1: Int, a2: Int): Int = a1 max a2

      override def empty: Int = 0
    }

    implicit def setInstance[A](): BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
      override def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 union a2

      override def empty: Set[A] = Set.empty[A]
    }
  }

  final case class GCounterWithStringKey[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GCounterWithStringKey[A] = {
      val value = amount |+| counters.getOrElse(machine, m.empty)
      GCounterWithStringKey(counters + (machine -> value))
    }

    def merge(that: GCounterWithStringKey[A])(implicit m: CommutativeMonoid[A]): GCounterWithStringKey[A] =
      GCounterWithStringKey(this.counters |+| that.counters)

    def total(implicit m: CommutativeMonoid[A]): A = counters.values.toList.combineAll
  }

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]

    def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
  }

  object GCounter {
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter

    implicit def mapGCounterInstance[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
      def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
        val total = f.getOrElse(k, m.empty) |+| v
        f + (k -> total)
      }

      def merge(map1: Map[K, V], map2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] =
        map1 |+| map2

      def total(map: Map[K, V])(implicit m: CommutativeMonoid[V]): V =
        map.values.toList.combineAll
    }
  }

  trait KeyValueStore[F[_, _]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](f: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)

    def values[K, V](f: F[K, V]): List[V]
  }

  implicit val mapKeyValueStoreInstance: KeyValueStore[Map] = new KeyValueStore[Map] {
    def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

    def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V = f.getOrElse(k, default)

    def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] = kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] = kvs.get(f)(key)

    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
  }

  implicit def gcounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]): GCounter[F, K, V] =
    new GCounter[F, K, V] {
      def increment(f: F[K, V])(key: K, value: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
        val total = f.getOrElse(key, m.empty) |+| value
        f.put(key, total)
      }

      def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2

      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.combineAll
    }

  def main(args: Array[String]): Unit = {
    import cats.instances.int._

    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)
    val counter = GCounter[Map, String, Int]
    val merged = counter.merge(g1, g2)
    val total = counter.total(merged)
    println(total)
  }
}
