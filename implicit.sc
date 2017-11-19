object Ex1 {

  class Foo

  class Bar {
    def who() = {
      println("Ex1 Foo")
    }
  }

  implicit def fooToBar(foo: Foo): Bar = new Bar
}

import Ex1._

val foo: Ex1.Bar = new Ex1.Foo
foo.who()


object Ex2 {

  trait Speakable[T] {
    def speak(t: T): String
  }

  def speak[T](t: T)(implicit M: Speakable[T]): String = {
    M.speak(t)
  }

  case class Foo(name: String)

}

import Ex2._

implicit object FooSeakable extends Speakable[Ex2.Foo] {
  def speak(foo: Ex2.Foo): String = foo.name
}

speak(Ex2.Foo("foo"))


object Ex3 {

  trait SumM[A] {
    def init: A

    def acc(a1: A, a2: A): A
  }

  object SumM {

    implicit object IntSum extends SumM[Int] {
      def init: Int = 0

      def acc(a1: Int, a2: Int): Int = a1 + a2
    }

    implicit object StringSum extends SumM[String] {
      def init: String = ""

      def acc(a1: String, a2: String): Int = a1 + a2
    }

  }

  trait FoldLeft[M[_]] {
    def foldLeft[A, B](xs: M[A])(b: B)(f: (B, A) => B): B
  }

  object FoldLeft {

    implicit object ListFoldLeft extends FoldLeft[List] {
      override def foldLeft[A, B](xs: List[A])(b: B)(f: (B, A) => B): B = xs.foldLeft(b)(f)
    }

  }

  def sum[M[_], A](xs: M[A])(implicit m: SumM[A], f: FoldLeft[M]): A = f.foldLeft(xs)(m.init)(m.acc)

  def +[A: SumM](a1: A, a2: A): A = implicitly[SumM[A]].acc(a1, a2)
}

import Ex3._

sum(List(1, 2, 3))
