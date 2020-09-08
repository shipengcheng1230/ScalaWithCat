package chapter1

object AnatomyOfATypeClass {

  sealed trait Json

  final case class JsObject(get: Map[String, Json]) extends Json

  final case class JsString(get: String) extends Json

  final case class JsNumber(get: Double) extends Json

  final case object JsNull extends Json

  trait JsonWriter[A] {
    def write(value: A): Json
  }

  final case class Person(name: String, email: String)

  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] =
      (value: String) => JsString(value)

    implicit val personWriter: JsonWriter[Person] =
      (value: Person) =>
        JsObject(Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email))
        )

    implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] = {
      case Some(aValue) => writer.write(aValue)
      case None => JsNull
    }
  }

  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

  object JsonSyntax {
    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
    }
  }

  def main(args: Array[String]): Unit = {
    import JsonWriterInstances._
    import JsonSyntax._

    Json.toJson(Person("Dave", "dave@example.com"))
    Person("Dave", "dave@example.com").toJson
    Json.toJson(Option("A string"))
  }
}
