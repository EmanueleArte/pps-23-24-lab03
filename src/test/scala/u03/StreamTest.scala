package u03

import org.junit.*
import org.junit.Assert.*
import u03.Streams.*
import Stream.*
import u03.Sequences.*
import Sequence.*
import org.junit.jupiter.api.Assertions.assertAll


class StreamTest:

  @Test def testIterate(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Nil())))), toList(Stream.take(str1)(4)))

  @Test def testMap(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
    assertEquals(Cons(1, Cons(2, Cons(3, Cons(4, Nil())))), toList(Stream.take(str2)(4)))

  @Test def testFilter(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.filter(str1)(x => x % 2 == 1) // {1,3,5,7,..}
    assertEquals(Cons(1, Cons(3, Cons(5, Cons(7, Nil())))), toList(Stream.take(str2)(4)))

  @Test def takeWhile(): Unit =
    val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
    val str2 = Stream.takeWhile(str1)(_ < 5) // {0,1,2,3,4}
    assertEquals(Cons(0, Cons(1, Cons(2, Cons(3, Cons(4, Nil()))))), Stream.toList(str2))

  @Test def testFill(): Unit =
    val str1 = Stream.fill(0)("x") // {}
    val str2 = Stream.fill(5)("x") // {"x","x","x","x","x"}
    assertAll(
      () => assertEquals(Nil(), toList(str1)),
      () => assertEquals(Cons("x", Cons("x", Cons("x", Cons("x", Cons("x", Nil()))))), toList(str2))
    )

  @Test def testPell(): Unit =
    assertAll(
      () => assertEquals(Nil(), Stream.toList(Stream.take(pell)(0))),
      () => assertEquals(Cons(0, Cons(1, Cons(2, Cons(5, Cons(12, Nil()))))), Stream.toList(Stream.take(pell)(5)))
    )