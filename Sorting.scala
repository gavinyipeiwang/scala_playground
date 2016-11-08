object Sorting {


  def mergeSort(xs: Array[Int]): Array[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(left: Array[Int], right: Array[Int], a: Array[Int]): Array[Int] = {
        var i, j, k = 0
        while (i < left.length && j < right.length) {
          if (left(i) < right(j)) {
            a(k) = left(i)
            i += 1
          } else {
            a(k) = right(j)
            j += 1
          }
          k += 1
        }
        a
      }
      val (l, r) = xs.splitAt(n)
      merge(mergeSort(l), mergeSort(r), xs)
    }
  }

  def quickSort(xs: Array[Int]): Array[Int] = {
    if (xs.length < 2) xs
    else {
      val pivot = xs(xs.length / 2)
      val (l, r) = xs.partition(_ < pivot)
      quickSort(l) ++ Array(pivot) ++ quickSort(r)
    }
  }

  def selectionSort(xs: Array[Int]): Array[Int] = {
    for (i <- 0 until xs.length) {
      val mi = _minFrom(xs, i)
      _swap(xs, i, mi)
    }
    xs
  }

  def bubbleSort(xs: Array[Int]): Array[Int] = {
    for (i <- 0 until xs.length;
         j <- 0 until xs.length - i - 1) {
      if (xs(j) > xs(j + 1)) {
        _swap(xs, j, j + 1)
      }
    }
    xs
  }

  def insertionSort(xs: Array[Int]): Array[Int] = {
    for (i <- 1 until xs.length) {
      val x = xs(i)
      var insertTo = i
      while (insertTo > 0 && xs(insertTo - 1) > x) {
        xs(insertTo) = xs(insertTo - 1)
        insertTo = insertTo - 1
      }
      xs(insertTo) = x
    }
    xs
  }


  private def _minWithIndex(a: (Int, Int), b: (Int, Int)): (Int, Int) = if (a._1 < b._1) a else b

  private def _maxWithIndex(a: (Int, Int), b: (Int, Int)): (Int, Int) = if (a._1 > b._1) a else b

  private def _minFrom(xs: Array[Int], from: Int): Int = {
    var min = xs(from)
    var mi = from
    for (i <- from until xs.length) {
      if (xs(i) < min) {
        min = xs(i)
        mi = i
      }
    }
    mi
  }

  private def _swap(xs: Array[Int], from: Int, to: Int): Unit = {
    val tmp = xs(from)
    xs(from) = xs(to)
    xs(to) = tmp
  }

}
