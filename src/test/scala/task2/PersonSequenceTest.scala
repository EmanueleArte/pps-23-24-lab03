package task2

import org.junit.*
import org.junit.Assert.*

import u03.extensionmethods.Sequences.*
import Sequence.*

import u02.Modules.*
import Person.*

class PersonSequenceTest:

  val people = Cons(Teacher("t1", "web"), Cons(Student("s1", 2024), Cons(Teacher("t2", "pps"), Nil())))

  @Test def testEmptySequence(): Unit =
    assertEquals(Nil(), Nil().courses)

  @Test def testEmptyCoursesSequence(): Unit =
    assertEquals(Nil(), Cons(Student("s1", 2023), Cons(Student("s2", 2024), Cons(Student("s3", 2024), Nil()))).courses)

  @Test def testCoursesSequence(): Unit =
    assertEquals(Cons("web", Cons("pps", Nil())), people.courses)
