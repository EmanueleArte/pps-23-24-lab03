package u03.extensionmethods

import u02.Modules.*
import Person.*

object Sequences:
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    extension (l: Sequence[Int])
      def sum: Int = l match
        case Cons(h, t) => h + t.sum
        case _          => 0

    extension [A](l: Sequence[A])

      def map[B](mapper: A => B): Sequence[B] = l match
        case Cons(h, t) => Cons(mapper(h), t.map(mapper))
        case Nil()      => Nil()

      def filter(pred: A => Boolean): Sequence[A] = l match
        case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
        case Cons(_, t)            => t.filter(pred)
        case Nil()                 => Nil()

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = l match
        case Cons(h, t) => concat(mapper(h), this.flatMap(t)(mapper))
        case _ => Nil()

      def foldLeft(dv: A)(pred: (A, A) => A): A = l match
        case Cons(h, t) => t.foldLeft(pred(dv, h))(pred)
        case Nil() => dv

    extension [Person](l: Sequence[Person])
      def courses: Sequence[String] = flatMap(l) {
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      }

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Cons(h, t), _) => Cons(h, concat(t, l2))
      case (Nil(), _) => l2

    def of[A](n: Int, a: A): Sequence[A] =
      if (n == 0) then Nil[A]() else Cons(a, of(n - 1, a))


@main def trySequences() =
  import Sequences.*
  import Sequence.*
  
  val seq = Cons(10, Cons(20, Cons(30, Nil())))
  println(seq.filter(_ >= 20).map(_ + 1).sum) // 21+31 = 52
  println(sum(map(filter(seq)(_ >= 20))(_ + 1))) // equally possible
  val seq2 = of(10, -1) // Cons(-1, Cons(-1, Cons(-1, ...)))
  println(seq2.sum) // -10
  
