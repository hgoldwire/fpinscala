def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
  def go(i: Int): Boolean = {
    if (i >= as.length - 1)
      true
    else {
      if (gt(as(i), as(i + 1)))
        go(i + 1)
      else
        false
    }
  }

  go(0)
}

def cmpInt(a: Int, c: Int): Boolean = a <= c

cmpInt(3, 5)
cmpInt(5, 3)

val a = Array()
val b = Array(3, 5)
val c = Array(5, 3)
val d = Array(2, 7, 5, 3)

isSorted(a, cmpInt)
isSorted(b, cmpInt)
isSorted(c, cmpInt)
isSorted(d, cmpInt)
isSorted(d.sorted, cmpInt)

isSorted(d, (a: Int, b:Int) => a <= b)
isSorted(d.sorted, (a: Int, b:Int) => a <= b)

isSorted(Array("a", "b", "c"), (a: String, b:String) => a <= b)