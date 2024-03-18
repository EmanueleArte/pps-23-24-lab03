package task2

import u02.Modules.*
import Person.*
import u03.Sequences.*
import Sequence.*

def getCourses(l: Sequence[Person]): Sequence[String] = flatMap(l) {
  case Teacher(_, c) => Cons(c, Nil())
  case _ => Nil()
}