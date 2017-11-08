package alvaro.collect

import scala.reflect.ClassTag

sealed trait A {
  val whatever: String
}

case class B(whatever: String) extends A

case class C(whatever: String) extends A

case class D(s: Seq[A]) {

  def filter[T <: A]()(implicit ev: ClassTag[T]): String =
    s.collect {
      case s: T => s.whatever
    }.head
}

object CollectSpec {

  def main(args: Array[String]) = {
    val d = D(Seq(B("this is a b"), C("this is a c")))
    println(d.filter[B])
    println(d.filter[C])
  }

}
