package task2

import org.junit.*
import org.junit.Assert.*

import u02.Modules.Person.*
import task2.getCourses

class PersonSequenceTest:

  import u03.Sequences.*
  import Sequence.*

  val people = Cons(Teacher("t1", "web"), Cons(Student("s1", 2024), Cons(Teacher("t2", "pps"), Nil())))

  @Test def testEmptySequence(): Unit =
    assertEquals(Nil(), getCourses(Nil()))

  @Test def testEmptyCoursesSequence(): Unit =
    assertEquals(Nil(), getCourses(Cons(Student("s1", 2023), Cons(Student("s2", 2024), Cons(Student("s3", 2024), Nil())))))

  @Test def testCoursesSequence(): Unit =
    assertEquals(Cons("web", Cons("pps", Nil())), getCourses(people))
