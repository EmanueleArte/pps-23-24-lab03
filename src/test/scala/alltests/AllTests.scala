package alltests

class SequenceTest:
  import u03.Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum(): Unit =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testTake(): Unit =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))

  @Test def testZip(): Unit =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))), zip(l, l2))
    assertEquals(Nil(), zip(l, Nil()))
    assertEquals(Nil(), zip(Nil(), l2))
    assertEquals(Nil(), zip(Nil(), Nil()))

  @Test def testConcat(): Unit =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), concat(l, l2))
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil(), l2))

  @Test def testFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin(): Unit =
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testMapWithFlatMap(): Unit =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapWithFlatMap(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), mapWithFlatMap(l)(_ + ""))

  @Test def testFilterWithFlatMap(): Unit =
    assertEquals(Cons(20, Cons(30, Nil())), filterWithFlatMap(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterWithFlatMap(l)(_ != 20))


class PersonSequenceTest:

  val people = Cons(Teacher("t1", "web"), Cons(Student("s1", 2024), Cons(Teacher("t2", "pps"), Nil())))

  @Test def testEmptySequence(): Unit =
    assertEquals(Nil(), Nil().courses)

  @Test def testEmptyCoursesSequence(): Unit =
    assertEquals(Nil(), Cons(Student("s1", 2023), Cons(Student("s2", 2024), Cons(Student("s3", 2024), Nil()))).courses)

  @Test def testCoursesSequence(): Unit =
    assertEquals(Cons("web", Cons("pps", Nil())), people.courses)


class FoldLefTest:

  val lst: Sequence[Int] = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test def testEmptyFoldLeft(): Unit =
    assertEquals(0, Nil().foldLeft(0)(_ + _))

  @Test def testFoldLeft(): Unit =
    assertEquals(-16, lst.foldLeft(0)(_ - _))


class StreamTest:
  import u03.Streams.*
  import Stream.*

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