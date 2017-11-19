val list = List(1, 2, 3, 4, 5)


def nItem(n: Int, list: List[Int]): List[Int] = list match {
  case _ if n == 0 => List.empty[Int]
  case _ if n > list.size => list ++ nItem(n - list.size, list)
  case Nil => List.empty[Int]
  case x :: xs => x :: nItem(n - 1, xs)
}


trait DL {

  def f[A](a: A): A

  def g[A](a: A): A

  val h = f _ compose g _
}

// def f(a:A):Mb
// def g(a:A):Mb
//
trait Monad[A] {
  def map[B](f: A => B): Monad[B]

  def flatMap[B](f: A => Monad[B]): Monad[B]
}

trait Foo {
  def bars: List[Bar]
}

trait Bar {
  def bazs: List[Baz]
}

trait Baz {
  def computeAll: List[Int]
}

def computeAll(foos: List[Foo]): List[Int] = {
  for {
    foo <- foos
    bar <- foo.bars
    baz <- bar.bazs
    results <- baz.computeAll
  } yield results
}