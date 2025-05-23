package ex1

import scala.annotation.tailrec

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None
  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)

  // Exercise: implement the following methods
  def zipWithValue[B](value: B): List[(A, B)] =
    foldRight(Nil())((_, value) :: _)
  def length(): Int =
    foldLeft(0)((acc, _) => acc + 1)
  def zipWithIndex: List[(A, Int)] =
    foldRight(Nil[(A, Int)](), length() - 1)((h, acc) => ((h, acc._2) :: acc._1, acc._2 - 1))._1
  def zipWithIndexInefficient: List[(A, Int)] =
    foldRight(Nil())((h, acc) => (h, this.length() - acc.length() - 1) :: acc)
  def partition(predicate: A => Boolean): (List[A], List[A]) =
    foldRight(Nil(), Nil())((h, acc) => if predicate(h) then (h :: acc._1, acc._2) else (acc._1, h :: acc._2))
  def partitionWithFilter(predicate: A => Boolean): (List[A], List[A]) =
    (filter(predicate), filter(!predicate(_)))
  def span(predicate: A => Boolean): (List[A], List[A]) =
    @tailrec
    def _span(acc: List[A], rest: List[A]): (List[A], List[A]) = rest match
      case h :: t if predicate(h) => _span(h :: acc, t)
      case _ => (acc, rest)
    _span(Nil(), this)
  def untilDo[B](init: B)(splitter: A => Boolean)(op: (A, B) => B): B = this match
    case h :: t if splitter(h) => t.untilDo(op(h, init))(splitter)(op)
    case _ => init
  def spanV2(predicate: A => Boolean): (List[A], List[A]) =
    untilDo((Nil(), this))(predicate)((h, acc) => (h :: acc._1, acc._2.tail.get))
  def takeRight(n: Int): List[A] =
    foldRight(Nil[A](), n)((h, acc) => (if acc._2 > 0 then h :: acc._1 else acc._1, acc._2 - 1))._1
  def collect(predicate: PartialFunction[A, A]): List[A] =
    foldRight(Nil())((h, acc) => if predicate.isDefinedAt(h) then predicate(h) :: acc else acc)
  def foldRightBranch[B](init: B)(brancher: (A, B) => Boolean)(op1: (A, B) => B, op2: (A, B) => B = (_, acc: B) => acc): B =
    foldRight(init)((h, acc) => if brancher(h, acc) then op1(h, acc) else op2(h, acc))
  def partitionV2(predicate: A => Boolean): (List[A], List[A]) =
    foldRightBranch(Nil[A](), Nil[A]())((h, acc) => predicate(h))((h, acc) => (h :: acc._1, acc._2), (h, acc) => (acc._1, h :: acc._2))
  def takeRightV2(n: Int): List[A] =
    foldRightBranch(Nil[A](), n)((_, acc) => acc._2 > 0)((h, acc) => (h :: acc._1, acc._2 - 1))._1
  def collectV2(predicate: PartialFunction[A, A]): List[A] =
    foldRightBranch(Nil[A]())((h, acc) => predicate.isDefinedAt(h))((h, acc) => predicate(h) :: acc)
// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:

  import List.*
  val reference = List(1, 2, 3, 4)
  println(reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println(reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println(reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println(reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println(reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println(reference.reduce(_ + _)) // 10
  println(List(10).reduce(_ + _)) // 10
  println(reference.takeRight(3)) // List(2, 3, 4)
  println(reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)