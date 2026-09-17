// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

// Note: we are using our own lazy lists, not the standard library

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h,t) => Some(h())

  def tail: LazyList[A] = this match
    case Empty => Empty
    case Cons(h,t) => t()

  /* Note 1. f can return without forcing the tail
   *
   * Note 2. this is not tail recursive (stack-safe) It uses a lot of stack if
   * f requires to go deeply into the lazy list. So folds sometimes may be less
   * useful than in the strict case
   *
   * Note 3. We added the type C to the signature. This allows to start with a
   * seed that is a subtype of what the folded operator returns.
   * This helps the type checker to infer types when the seed is a subtype, for 
   * instance, when we construct a list:
   *
   * o.foldRight (Nil) ((a,z) => a:: z)
   *
   * The above works with this generalized trick. Without the C generalization
   * the compiler infers B to be List[Nothing] (the type of Nil) and reports
   * a conflict with the operator.  Then we need to help it like that:
   *
   * o.foldRight[List[Int]] (Nil) ((a,z) => a:: z)
   *
   * With the C type trick, this is not neccessary. As it hints the type
   * checker to search for generalizations of B.
   *
   * I kept the foldLeft type below in a classic design, so that you can
   * appreciate the difference. Of course, the same trick could've been
   * applied to foldLeft.
   */
  def foldRight[B, C >: B](z: => B)(f: (A, => C) => C): C = this match
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))

  /* Note 1. Eager; cannot be used to work with infinite lazy lists. So
   * foldRight is more useful with lazy lists (somewhat opposite to strict lists)
   * Note 2. Even if f does not force z, foldLeft will continue to recurse.
   */
  def foldLeft[B](z: => B)(f :(A, => B) => B): B = this match
    case Empty => z
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)

  // Note: Do you know why we can implement find with filter for lazy lists but
  // would not do that for regular lists?
  def find(p: A => Boolean) = 
    this.filter(p).headOption

  // Exercise 2

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h()::t().toList 
  

  // Test in the REPL, for instance: LazyList(1,2,3).toList 
  // (and see what list is constructed)

  // Exercise 3
  // Our take function only ever creates one Cons and perhaps checks one if statement, containing the formular for the rest, but not forcing anything. Thus is matters not how large the number is.
  // Drop does not force the head, but does force the tail, and toList forces both head and tail.
  // So only the size of drop and toList matters.

  def take(n: Int): LazyList[A] = this match
    case Empty => Empty
    case Cons(h,t) => if n > 0 then Cons(h, () => t().take(n-1)) else Empty

  def drop(n: Int): LazyList[A] = this match
    case Empty => Empty
    case Cons(h,t) => if n > 0 then t().drop(n-1) else Cons(h, t)

  // Exercise 4
  // Same structure as take. We check the if, we make the new lazyList, and the calculate nothing else until forced. So again the size of takeWhile matters not.
  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Empty => Empty
    case Cons(h,t) => if p(h()) then Cons(h, () => t().takeWhile(p)) else Empty

  // Exercise 5
  // If we dont know the answer, we could potentially keep going forever with a infinte list, since we force head and tail 
  def forAll(p: A => Boolean): Boolean = this match
    case Empty => true
    case Cons(h,t) => if p(h()) then t().forAll(p) else false
 
  // Note 1. lazy; tail is never forced if satisfying element found this is
  // because || is non-strict
  // Note 2. this is also tail recursive (because of the special semantics
  // of ||)
  def exists(p: A => Boolean): Boolean = this match
    case Empty => false
    case Cons(h,t) => if p(h()) then true else t().exists(p)

  // Exercise 6
  
  def takeWhile1(p: A => Boolean): LazyList[A] = this.foldRight[LazyList[A], LazyList[A]](Empty) ((x,y) => if p(x) then cons(x,y) else Empty)

  // Exercise 7
  
  def headOption1: Option[A] = this.foldRight(None)((x, y) => Some(x))

  // Exercise 8
  
  // Note: The type is incorrect, you need to fix it
  def map[B](f: A => B): LazyList[B] = this.foldRight[LazyList[B], LazyList[B]](Empty)((x, y) => cons(f(x), y))
  
  // Note: The type is incorrect, you need to fix it
  def filter(p: A => Boolean): LazyList[A] = this.foldRight[LazyList[A], LazyList[A]](Empty)((x,y) => if p(x) then cons(x,y) else y)

  /* Note: The type is given correctly for append, because it is more complex.
   * Try to understand the type. The contsraint 'B >: A' requires that B is a
   * supertype of A. The signature of append allows to concatenate a list of
   * supertype elements, and creates a list of supertype elements.  We could have
   * writte just the following:
   *
   * def append(that: => LazyList[A]): LazyList[A]
   *
   * but this would not allow adding a list of doubles to a list of integers
   * (creating a list of numbers).  Compare this with the definition of
   * getOrElse last week, and the type of foldRight this week.
   */
  def append[B >: A](that: => LazyList[B]): LazyList[B] = this.foldRight[LazyList[B], LazyList[B]](that)((x,y) => cons(x,y))

  // Note: The type is incorrect, you need to fix it
  def flatMap[B](f: A => LazyList[B]): LazyList[B] = this.foldRight[LazyList[B], LazyList[B]](Empty)((x, y) => f(x).append(y))

  // Exercise 9
  // Type answer here
  // Filter is lazy, do only the work required.
  // HeadOption requires only a head
  // So filter will go through the list until it finds a suitable head, and return it, doing no more work.
  // In a strict list, filter would go through the entire list before headOption is even called
  // Scroll down to Exercise 10 in the companion object below

  // Exercise 13

  def mapUnfold[B](f: A => B): LazyList[B] = unfold(this) {
    case Empty =>
      None
    case Cons(h, t) =>
      Some((f(h()), t()))
  }

  def takeUnfold(n: Int): LazyList[A] = unfold((n, this)) {
    case (remaining, Empty) =>
      None
    case (remaining, Cons(h, t)) =>
      if remaining > 0 then Some((h(), (remaining - 1, t()))) else None
  }

  def takeWhileUnfold(p: A => Boolean): LazyList[A] = unfold(this) {
    case Empty =>
      None
    case Cons(h, t) => if p(h()) then Some((h(), t())) else None
  }

  def zipWith[B >: A, C] (ope: (=> B, => B) => C) (bs: LazyList[B]): LazyList[C] = 
    ???

end LazyList // enum ADT



// The companion object for lazy lists ('static methods')

object LazyList:

  def empty[A]: LazyList[A] = Empty

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty 
    then empty
    else cons(as.head, apply(as.tail*))

  // Exercise 1

  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  def to(n: Int): LazyList[Int] = cons(n, to(n-1))

  lazy val naturals: LazyList[Int] = from(1)

  // Scroll up to Exercise 2 to the enum LazyList definition 
  
  // Exercise 10

  // Note: The type is incorrect, you need to fix it
  def fib(pre: Int, cur: Int): LazyList[Int] = cons(cur, fib(cur, pre+cur))

  lazy val fibs: LazyList[Int] = cons(0, fib(0,1))

  // Exercise 11

  def unfold[A,S](z: S)(f: S => Option[(A, S)]): LazyList[A] =
    (for
      (value, state) <- f(z)
    yield
      cons(value, unfold(state)(f))).getOrElse(Empty)

  // Exercise 12

  // Note: The type is incorrect, you need to fix it
  lazy val fibsUnfold: LazyList[Int] = unfold((0,1))((x,y) => Some((x, (y,x+y))))

  // Scroll up for Exercise 13 to the enum

end LazyList // companion object
