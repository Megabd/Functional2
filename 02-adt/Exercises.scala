// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.adt

import java.util.NoSuchElementException

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])


object List: 

  def head[A] (l: List[A]): A = l match
    case Nil => throw NoSuchElementException() 
    case Cons(h, _) => h                                                                                                                                                                                                                                       
  
  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A] (l1: List[A], l2: List[A]): List[A] =
    l1 match
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2)) 

  def foldRight[A, B] (l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(a, as) => f(a, foldRight(as, z, f))
    
  def map[A, B] (l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]] (l, Nil, (a, z) => Cons(f(a), z))

  // Exercise 1 (is to be solved without programming) 3

  // Exercise 2

  def tail[A] (l: List[A]): List[A] = l match
      case Nil => throw NoSuchElementException()
      case Cons(_, t) => t

  // Exercise 3
  
  def drop[A] (l: List[A], n: Int): List[A] = (l, n) match
    case (l, n) if n <= 0 => l
    case (Nil, _) => throw NoSuchElementException() 
    case (Cons(_, tail), n) => drop(tail, n-1)


  // Exercise 4
  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match
    case Nil => Nil
    case Cons(h, tail) => if p(h) then dropWhile(tail, p) else l

  // Exercise 5
 
  def init[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(h, Nil) => Nil
    case Cons(h, tail) => Cons(h, init(tail))
  

  // Exercise 6

  def length[A](l: List[A]): Int = foldRight(l, 0, (x, y) => y + 1)

  // Exercise 7
  @annotation.tailrec
  def foldLeft[A, B] (l: List[A], z: B, f: (B, A) => B): B = l match
    case Nil => z
    case Cons(h, tail) => foldLeft(tail, f(z, h), f)

  // Exercise 8

  def product (as: List[Int]): Int = foldLeft(as, 1, _*_)

  def length1[A] (as: List[A]): Int = foldLeft(as, 0, (x, y) => x + 1)

  // Exercise 9

  def reverse[A] (l: List[A]): List[A] = foldLeft(l, Nil, (x, y) => Cons(y,x))
 
  // Exercise 10

  def foldRight1[A, B] (l: List[A], z: B, f: (A, B) => B): B = foldLeft(reverse(l), z, (x, y) => f(y, x))

  // Exercise 11

  def foldLeft1[A, B] (l: List[A], z: B, f: (B, A) => B): B = ???

 
  // Exercise 12

  def concat[A] (l: List[List[A]]): List[A] = l match
    case Nil => Nil
    case Cons(head, tail) => append(head, concat(tail))
  
  
  // Exercise 13

  def filter[A] (l: List[A], p: A => Boolean): List[A] = l match
    case Nil => Nil
    case Cons(head, tail) => if p(head) then Cons(head, filter(tail, p)) else filter(tail, p)
  
 
  // Exercise 14

  def flatMap[A,B] (l: List[A], f: A => List[B]): List[B] = 
    foldRight[A, List[B]] (l, Nil, (x, y) => append(f(x),y))

  // Exercise 15

  def filter1[A](l: List[A], p: A => Boolean): List[A] = flatMap(l, x => if p(x) then List(x) else Nil)
  

  // Exercise 16

  def addPairwise (l: List[Int], r: List[Int]): List[Int] = (l, r) match
    case (Nil, Nil) => Nil
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(xHead, xTail), Cons(yHead, yTail)) => Cons(xHead + yHead, addPairwise(xTail, yTail))
  

  // Exercise 17

  def zipWith[A, B, C] (l: List[A], r: List[B], f: (A,B) => C): List[C] = (l, r) match
    case (Nil, Nil) => Nil
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(xHead, xTail), Cons(yHead, yTail)) => Cons(f(xHead, yHead), zipWith(xTail, yTail, f))

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = ???
