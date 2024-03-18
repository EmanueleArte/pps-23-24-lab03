package task2

import org.junit.*
import org.junit.Assert.*

import u03.extensionmethods.Sequences.*
import Sequence.*

class FoldLefTest:

  val lst: Sequence[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test def testEmptyFoldLeft(): Unit =
    assertEquals(0, Nil().foldLeft(0)(_ + _))

  @Test def testFoldLeft(): Unit =
    assertEquals(-16, lst.foldLeft(0)(_ - _))
