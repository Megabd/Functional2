// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.option

// Exercise 1

trait OrderedPoint
  extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>

  override def compare(that: java.awt.Point): Int = 
    if this.x > that.x then 1
    else if this.x < that.x then -1
    else if this.y > that.y then 1
    else if this.y < that.y then -1
    else 0


// Try the following (and similar) tests in the repl (sbt console):
//
// import adpro.option.*
// val p = new java.awt.Point(0, 1) with OrderedPoint
// val q = new java.awt.Point(0, 2) with OrderedPoint
// assert(p < q)



// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:

  // Exercise 2

  def size[A](t: Tree[A]): Int = t match
    case Leaf(value) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  

  // Exercise 3

  def maximum(t: Tree[Int]): Int = t match
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)

  // Exercise 4

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) =>Branch(map(left)(f), map(right)(f))

  // Exercise 5

  def fold[A,B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = t match
    case Leaf(value) => g(value)
    case Branch(left, right) => f(fold(left)(f)(g), fold(right)(f)(g)) 
  

  def size1[A](t: Tree[A]): Int = fold[A, Int](t)((a, b) => a+b+1)(_ => 1)

  def maximum1(t: Tree[Int]): Int = fold[Int, Int](t)((a,b) => a max b)(a => a)

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)((a,b) => Branch(a,b))(a => Leaf(f(a)))




enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] = this match
  case None => None
  case Some(value) => Some(f(value))

  def getOrElse[B >: A] (default: => B): B = this match
  case None => default
  case Some(value) => value

  def flatMap[B](f: A => Option[B]): Option[B] =  this match
  case None => None
  case Some(value) => f(value)

  def filter(p: A => Boolean): Option[A] = this match
  case None => None
  case Some(value) => if p(value) then Some(value) else None

  // Scroll down for Exercise 7, in the bottom of the file, outside Option

  def forAll(p: A => Boolean): Boolean = this match
    case None => true
    case Some(x) => p(x)

end Option

// Exercise 9

def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] =
  for
    x <- ao
    y <- bo
  yield
    f(x,y)

// Exercise 10

def sequence[A](aos: List[Option[A]]): Option[List[A]] =  aos.foldRight[Option[List[A]]](Some(Nil)) ((elem,list) => map2(elem,list)((x,y) => x::y))

// Exercise 11

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as.foldRight[Option[List[B]]](Some(Nil)) ((elem,list) => map2(f(elem),list)((x,y) => x::y))



// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil => None
  case h:: t => Some(h)

// Exercise 7

def headGrade(lst: List[(String,Int)]): Option[Int] = headOption(lst).map(x => x._2)

def headGrade1(lst: List[(String,Int)]): Option[Int] =
  for 
    x <- headOption(lst)
  yield 
    x._2

// Implemented in the text book

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercise 8

def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(x => mean(xs.map(y => (y-x)*(y-x))))

def variance1(xs: Seq[Double]): Option[Double] =
  for
    myMean <- mean(xs)
    varianceList = xs.map(x => (x-myMean)*(x-myMean))
    finalVariance <- mean(varianceList)
  yield
    finalVariance
    
// Scroll up, to the Option object for Exercise 9
