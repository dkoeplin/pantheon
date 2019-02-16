package utils.implicits

import scala.collection.mutable

/**
  * General helper methods for various data structures in Scala.
  */
object collections {

//  TODO: Relocate elsewhere
//  implicit class IntSeqHelpers(x: Seq[Int]) {
//    /** Returns a sequence containing only elements who are unique in mod N. */
//    def uniqueModN(N: Int): Seq[Int] = {
//      var map = Map.empty[Int,Int]
//      x.foreach{i => if (!map.contains(i%N)) map += i%N -> i }
//      map.values.toList.sorted
//    }
//  }

  implicit class SeqHelpers[A](x: Seq[A]) {
    def get(i: Int): Option[A] = if (i >= 0 && i < x.length) Some(x(i)) else None
    def indexOrElse(i: Int, els: => A): A = if (i >= 0 && i < x.length) x(i) else els

    private def inter(left: Boolean, x: Seq[A], y: Seq[A]): Seq[A] = (left,x,y) match {
      case (_, Nil, Nil)   => Nil
      case (true, Nil, _)  => y
      case (true, _, _)    => x.head +: inter(!left, x.tail, y)
      case (false, _, Nil) => x
      case (false, _, _)   => y.head +: inter(!left, x, y.tail)
    }
    
    def interleave(y: Seq[A]): Seq[A] = inter(left=true,x,y)

    /** Returns a new Map from elements in x to func(x)  */
    def mapping[B](func: A => B): Map[A,B] = x.map{x => x -> func(x) }.toMap

    def collectAsMap[B,C](func: PartialFunction[A,(B,C)]): Map[B,C] = x.collect(func).toMap

    /** Returns true if the length of x is exactly len, false otherwise.
      * Equivalent to (but faster than) x.length == len
      */
    def lengthIs(len: Int): Boolean = x.lengthCompare(len) == 0

    /** Returns true if length of x is less than len, false otherwise.
      * Equivalent to (but faster than) x.length < len
      */
    def lengthLessThan(len: Int): Boolean = x.lengthCompare(len) < 0

    /** Returns true if length of x is more than len, false otherwise
      * Equivalent to (but faster than) x.length > len
      */
    def lengthMoreThan(len: Int): Boolean = x.lengthCompare(len) > 0

    def mapFind[B](func: A => Option[B]): Option[B] = {
      var res: Option[B] = None
      val iter = x.iterator
      while (res.isEmpty && iter.hasNext) {
        res = func(iter.next())
      }
      res
    }

    /** Returns the tail of this sequence, or Nil if this is an empty collection. */
    def tailOrNil: Seq[A] = if (x.isEmpty) Nil else x.tail
  }

  implicit class IterableHelpers[A](x: Iterable[A]) {
    /** Returns a new Map from elements in x to func(x)  */
    def mapping[B](func: A => B): Map[A,B] = x.map{x => x -> func(x) }.toMap

    def maxByOrElse[B:Ordering](z: A)(f: A => B): A = if (x.isEmpty) z else x.maxBy(f)
    def minByOrElse[B:Ordering](z: A)(f: A => B): A = if (x.isEmpty) z else x.minBy(f)

    def minOrElse(z: A)(implicit o: Ordering[A]): A = if (x.isEmpty) z else x.min
    def maxOrElse(z: A)(implicit o: Ordering[A]): A = if (x.isEmpty) z else x.max

    def cross[B](y: Iterable[B]): Iterator[(A,B)] = {
      x.iterator.flatMap{a => y.iterator.map{b => (a,b) } }
    }

    /**
      * Returns true if the given function is true over all combinations of 2 elements
      * in this collection.
      */
    def forallPairs(func: (A,A) => Boolean): Boolean = x.pairs.forall{case (a,b) => func(a,b) }

    /**
      * Returns an iterator over all combinations of 2 from this iterable collection.
      * Assumes that either the collection has (functionally) distinct elements.
      */
    def pairs: Iterator[(A,A)] = {
      if (x.size >= 2) x.tail.iterator.map{m1 => (x.head,m1) } ++ x.tail.pairs
      else Iterator.empty
    }

    def mapFind[B](func: A => Option[B]): Option[B] = {
      var res: Option[B] = None
      val iter = x.iterator
      while (res.isEmpty && iter.hasNext) {
        res = func(iter.next())
      }
      res
    }
  }

  implicit class IteratorHelpers[A](x: Iterator[A]) {
    def mapFind[B](func: A => Option[B]): Option[B] = {
      var res: Option[B] = None
      val iter = x
      while (res.isEmpty && iter.hasNext) {
        res = func(iter.next())
      }
      res
    }
  }

  // set.getOrElseAdd{case v: T => v.field == thing}{ new T() }
  implicit class SetOps[V](set: mutable.Set[V]) {
    def getOrElseAdd[K<:V](pattern: PartialFunction[V,K])(els: => K): K = set.find{v => pattern.isDefinedAt(v) } match {
      case Some(v) => pattern(v)
      case None => val v = els; set += v; v
    }
  }

}
