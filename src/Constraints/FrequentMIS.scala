package Constraints

import utils.{ Constantes, ReversibleSparseBitSet2 }
import oscar.algo.Inconsistency
import oscar.algo.reversible._
import oscar.cp._
import oscar.cp.core.CPPropagStrength
import java.util.ArrayList
import java.util.Collection

class FrequentMIS(val I: Array[CPBoolVar], var S: CPIntVar, var mis: Array[Int], val nItems: Int, val nTrans: Int, TDB: Array[Set[Int]]) extends Constraint(I(0).store, "CoverSize") {
  idempotent = true

  //init coverage
  private[this] val coverage = new ReversibleSparseBitSet2(s, nTrans, 0 until nTrans)

  ///Create matrix B (nItems x nTrans) (i = item, j = transaction)
  //Is such that columns(i) is the coverage of item i.
  private[this] val columns = Array.tabulate(nItems) { x => new coverage.BitSet(TDB(x)) }

  ///contains all the unbound variables that are not in the closure of the current itemset.
  //closure => freq(I(D)U{i}) = freq(I(D))
  private[this] val unboundNotInClosureIndices = Array.tabulate(I.length)(i => i)
  private[this] val revUnboundNotInClosure = new ReversibleInt(s, I.length)
  //MISs lists of items grouped by MISs values
  val misl = scala.collection.mutable.Map[Int, java.util.ArrayList[Int]]()

  /**
   *
   * @param l
   * @return CPOutcome state
   */
  override def setup(l: CPPropagStrength): Unit = {
    for (i <- 0 until nItems; if !I(i).isBound) {
      I(i).callPropagateWhenBind(this)
    }
   
    propagate()
  }

  /**
   *
   * @return CPOutcome state
   */
  override def propagate(): Unit = {

    coverage.clearCollected()
    var nU = revUnboundNotInClosure.value

    //Compute MIS lists

    var i = nU
    while (i > 0) {
      i -= 1
      val idx = unboundNotInClosureIndices(i)
      if (!I(idx).isBoundTo(0)) {
        if (misl.contains(mis(idx))) {
          var temp = misl.get(mis(idx)).get
          if (!temp.contains(idx))
            temp.add(idx)
          misl.put(mis(idx), temp)
        } else {
          val temp = new java.util.ArrayList[Int]()
          temp.add(idx)
          misl.put(mis(idx), temp)
        }
      } else {
        misl.get(mis(idx)).get.remove(new Integer(idx))
        if (misl.get(mis(idx)).get.isEmpty())
          S.removeValue(mis(idx))
      }
    }

    i = nU
    while (i > 0) {
      i -= 1
      val idx = unboundNotInClosureIndices(i)
      if (I(idx).isBound) {

        nU = removeItem(i, nU, idx)

        //when bound to 1, then idx in coverage, make intersection
        if (I(idx).min == 1) {
          coverage.intersectWith(columns(idx))

        }
        //Remove mis(idx) if all variables associated to mis(idx) are set to 0
        if (I(idx).max == 0 && misl.get(mis(idx)).get.contains(idx)) {
          misl.get(mis(idx)).get.remove(new Integer(idx))
          if (misl.get(mis(idx)).get.isEmpty())
            S.removeValue(mis(idx))
        }
      }

    }

    var frequency = S.getMin
    val cardinality = coverage.count()
    Constantes.curSupport = cardinality

    //Pruning From CoverSize 2017
    if (cardinality == frequency) {
      Constantes.nSubCase += 1
      pruneIfSupEqFreq(nU)
    } else {
      prune(nU, cardinality)
    }

    revUnboundNotInClosure.value = nU

    //failure of frequency rule

    frequency = S.getMin

    if (cardinality < frequency)
      throw Inconsistency

  }

  /**
   *
   * @param item
   * @param nU    the number of not unbound item which are not in the current closure
   * @param index the index of current item
   * @return
   */
  def removeItem(item: Int, nU: Int, index: Int): Int = {
    val lastU = nU - 1
    unboundNotInClosureIndices(item) = unboundNotInClosureIndices(lastU)
    unboundNotInClosureIndices(lastU) = index
    lastU
  }

  /**
   *
   * @param nUbound     the number of not unbound item which are not in the current closure
   * @param cardinality the frequency (support) of current itemset
   * @return
   */
  def prune(nUbound: Int, cardinality: Int): Unit = {
    var nU = nUbound
    var i = nU
    var frequency = S.getMin
    while (i > 0) {
      i -= 1
      val idx = unboundNotInClosureIndices(i)
      val cardIdx = coverage.intersectCount(columns(idx), frequency)

      //!IMPORTANT : condition 1 & 2 will never happen together
      //condition 1 : cardIdx < freq => Idx infrequent
      if (cardIdx < frequency) {
        nU = removeItem(i, nU, idx)

        //enforced to zero, this item will not be taken into account anymore
        I(idx).removeValue(1)
        if (misl.get(mis(idx)).get.contains(idx)) {
          misl.get(mis(idx)).get.remove(new Integer(idx))
          if (misl.get(mis(idx)).get.isEmpty())
            S.removeValue(mis(idx))
        }
      } else if (cardIdx == cardinality) { //condition 2 : cardIdx = cardinality => freq(I(U)) = freq(I(U)+idx) => closure
        nU = removeItem(i, nU, idx)
      }
      frequency = S.getMin
    }
    revUnboundNotInClosure.value = nU
  }

  def pruneIfSupEqFreq(nUbound: Int): Unit = {

    var nU = nUbound
    var i = nU
    var frequency = S.getMin
    while (i > 0) {
      i -= 1
      val idx = unboundNotInClosureIndices(i)
      val cardIdx = coverage.intersectCount(columns(idx), frequency)

      // cond1: if coverage is supperset of item bitvector
      // => |their intersect| < frequency
      if (!coverage.isSubSetOf(columns(idx))) {
        nU = removeItem(i, nU, idx)
        //enforced to zero, this item will not be taken into account anymore
        I(idx).removeValue(1)
        if (misl.get(mis(idx)).get.contains(idx)) {
          misl.get(mis(idx)).get.remove(new Integer(idx))
          if (misl.get(mis(idx)).get.isEmpty())
            S.removeValue(mis(idx))
        }
      } else { //else |their intersect| = frequency => closure => enumeration by search
        nU = removeItem(i, nU, idx)
      }
      frequency = S.getMin
    }
    revUnboundNotInClosure.value = nU
  }
}