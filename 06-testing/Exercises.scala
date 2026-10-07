// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
//import lazyList02.* // uncomment to test another version

import LazyList.*

object LazyListSpec
  extends org.scalacheck.Properties("testing"):

  /* Generators and helper functions */

  /** Convert a strict list to a lazy-list */
  def list2lazyList[A](la: List[A]): LazyList[A] =
    LazyList(la*)

  /** Generate finite non-empty lazy lists */
  def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
    for la <- arbitrary[List[A]].suchThat { _.nonEmpty }
    yield list2lazyList(la)

  /** Generate an infinite lazy list of A values.
    *
    * This lazy list is infinite if the implicit generator for A never fails. The
    * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
    * not tail recursive)
    */
  def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
    def loop: LazyList[A] =
      summon[Arbitrary[A]].arbitrary.sample match
        case Some(a) => cons(a, loop)
        case None => empty
    Gen.const(loop)

  /* The test suite */

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") =
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random"

  // Exercise 2
  property("Ex02.01: headOption does not force the tail of a lazy list.") =
    val myList = cons(1, throw new RuntimeException("tail forced"))
    myList.headOption == Some(1)


  // Exercise 3
  property("Ex03.01: take does not force any heads nor any tails of the lazy list it manipulates") =
    val myList = cons(throw new RuntimeException("head forced") ,throw new RuntimeException("tail forced"))
    myList.take(1)
    true

  // Exercise 4
  property("Ex04.01: take(n) does not force the (n+1)st head ever ") =
    forAll { (n: Int) => (n>= 0) ==> (bombAtEnd(n).take(n).toList == List.fill(n)(1))}

  def bombAtEnd (n : Int) : LazyList[Int] = n match
    case 0 => cons (throw new RuntimeException("n+1 head was forced"), empty)
    case m => cons (1, bombAtEnd(m-1))
  

  // Exercise 5
  property("Ex05.01: l.take(n).take(n) == l.take(n) for any lazy list s and any n") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (l: LazyList[Int], n: Int) => (n >= 0) ==> (l.take(n).take(n) == l.take(n))}
    
  property("Ex05.02: empty") =
    forAll { (n: Int) => (n >= 0) ==> (empty.take(n).take(n) == empty.take(n))}

  // Exercise 6
  property("Ex06.01: l.drop(n).drop(m) == l.drop(n+m) for any n, m") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (l: LazyList[Int], n: Int, m: Int) => (n >= 0 && m >= 0) ==> (l.drop(n).drop(m) == l.drop(n+m))}

  property("Ex06.02: empty") =
    forAll { (n: Int, m: Int) => (n >= 0 && m >= 0) ==> (empty.drop(n).drop(m) == empty.drop(n + m))}

  // Exercise 7
  property("Ex07.01: l.drop(n) does not force any of the dropped elements") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (l: LazyList[Int], n: Int) =>  (n >= 0) ==> (addNBombsToHead(n, l).drop(n).headOption == l.headOption)}

  def addNBombsToHead(n: Int, tail: LazyList[Int]): LazyList[Int] = n match 
  case 0 => tail
  case m => cons(throw new RuntimeException("head forced"), addNBombsToHead(m - 1, tail))
  // Exercise 8
  property("Ex08.01: l.map(identity) == l for any lazy list l)") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (l: LazyList[Int]) => l.map(identity) == l}

  property("Ex08.02: empty") =
    empty.map(identity) == empty

  // Exercise 9
  property("Ex09.01: map terminates on infinite lazy lists") =
  forAll(infiniteLazyList[Int]) { l =>
    l.map(identity)
    true
  }
  
  // Exercise 10
  property("Ex10.01: appending empty changes nothing") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (l: LazyList[Int]) => l.append(empty) == l}

  property("Ex10.02: appending to empty gives the appended list") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (l: LazyList[Int]) => empty.append(l) == l}

  property("Ex10.03: append contains first list followed by second list") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll { (l1: LazyList[Int], l2: LazyList[Int]) => l1.append(l2).toList == l1.toList ++ l2.toList}