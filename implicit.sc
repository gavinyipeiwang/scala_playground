object Ex1{
  object App1 {
    class Foo
    class Bar {
      def who() = {
        println("Ex1 Foo")
      }
    }
    implicit def fooToBar(foo: Foo):Bar = new Bar
  }
  import App1._

  val foo:Bar = new Foo
  foo.who()
}

