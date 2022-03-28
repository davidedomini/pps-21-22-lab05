package u05lab.ex1

import u05lab.ex1.List

// Ex 1. implement the missing methods both with recursion or with using fold, map, flatMap, and filters
// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)
    case _ => None

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight[List[B]](Nil())(f(_) append _)

  def foldLeft[B](z: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(z, h))(op)
    case Nil() => z

  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case h :: t => f(h, t.foldRight(z)(f))
    case _ => z

  def length: Int = foldLeft(0)((l, _) => l + 1)

  def isEmpty: Boolean = this match
    case Nil() => true
    case _ => false

  def reverse(): List[A] = foldLeft[List[A]](Nil())((l, e) => e :: l)

  /** EXERCISES */
  def zipRightWithRecursion: List[(A, Int)] =
    def _zip(i: Int, l: List[A]): List[(A, Int)] = l match
      case h :: t => (h, i) :: _zip(i+1, t)
      case Nil() => Nil()
    _zip(0, this)

  def zipRight: List[(A, Int)] = this.map((e) => (e, 0))

  def partition(pred: A => Boolean): (List[A], List[A]) =
    this.foldLeft((Nil(), Nil()))((acc, e) => if pred(e) then (acc._1.append(List(e)), acc._2) else (acc._1, acc._2.append(List(e))))

  def span(pred: A => Boolean): (List[A], List[A]) =
    val (x: List[A], y: List[A], z) = this.foldLeft((Nil(), Nil(), true))((acc, e) => if !pred(e) then if acc._3 then (acc._1.append(List(e)), acc._2, true) else (acc._1, acc._2.append(List(e)), false) else if acc._3 then (acc._1.append(List(e)), acc._2, false) else (acc._1, acc._2.append(List(e)), false))
    (x, y)

  def reduce(op: (A, A) => A): Option[A] = this match
    case _ if this.isEmpty => None
    case h :: t if t.isEmpty => Some(h)
    case h :: t  => Some(op(h, t.reduce(op).get))

  def takeRight(n: Int): List[A] = ???

  // k = 2

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

@main def checkBehaviour(): Unit =
  val reference = List(1, 2, 3, 4)
  println(reference.zipRight) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  try Nil.reduce[Int](_ + _)
  catch case ex: Exception => println(ex) // prints exception
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
