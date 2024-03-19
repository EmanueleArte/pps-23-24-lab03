package alltasks

import u03.Optionals.Optional

// Task 1 - Svolto da solo
object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

  // 1. --------------------------------
    // a)
    def take[A](l: Sequence[A])(n: Int): Sequence[A] = l match
      case Cons(h, t) if n > 0 => Cons(h, take(t)(n - 1))
      case _ => Nil()

    // b)
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
      case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
      case _                            => Nil()

    // c) -----------
    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Cons(h, t), _) => Cons(h, concat(t, l2))
      case (Nil(), _)      => l2

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] = l match
      case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
      case _          => Nil()
    // ---------------

    // d) -----------
    def mapWithFlatMap[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = flatMap(l)(x => Cons(mapper(x), Nil()))

    def filterWithFlatMap[A](l: Sequence[A])(pred: A => Boolean): Sequence[A] = flatMap(l)(x => if pred(x) then Cons(x, Nil()) else Nil())
    // ---------------
  // ------------------------------------

  // 2. --------------------------------
    def min(l: Sequence[Int]): Optional[Int] = l match
      case Cons(h, t) => t match
        case Cons(h2, t2) => min(t) match
          case Optional.Just(v) if v < h => Optional.Just(v)
          case _ => Optional.Just(h)
        case _ => Optional.Just(h)
      case _ => Optional.Empty()
  // ------------------------------------


// Task 2 - Svolto da solo
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

    // 5. --------------------------------
      // 3. -----------
      def foldLeft(dv: A)(pred: (A, A) => A): A = l match
        case Cons(h, t) => t.foldLeft(pred(dv, h))(pred)
        case Nil() => dv
      // --------------

      // 4. -----------
    extension [Person](l: Sequence[Person])
      def courses: Sequence[String] = flatMap(l) {
        case Teacher(_, c) => Cons(c, Nil())
        case _ => Nil()
      }
      // --------------
    // ------------------------------------

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] = (l1, l2) match
      case (Cons(h, t), _) => Cons(h, concat(t, l2))
      case (Nil(), _) => l2

    def of[A](n: Int, a: A): Sequence[A] =
      if (n == 0) then Nil[A]() else Cons(a, of(n - 1, a))


// Task 3 - Svolto da solo
object Streams extends App:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // 6. --------------------------------
    def takeWhile[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if pred(head()) => cons(head(), takeWhile(tail())(pred))
      case _ => Empty()
    // -----------------------------------

    // 7. --------------------------------
    def fill[A](n: Int)(k: A): Stream[A] = n match
      case n if n > 0 => cons(k, fill(n - 1)(k))
      case _ => Empty()
    // -----------------------------------

    // 8. --------------------------------
    val pell: Stream[Int] =
      def nextPell(n1: Int, n2: Int): Stream[Int] =
        val next = 2 * n2 + n1
        cons(next, nextPell(n2, next))

      cons(0, cons(1, nextPell(0, 1)))
    // -----------------------------------

  end Stream