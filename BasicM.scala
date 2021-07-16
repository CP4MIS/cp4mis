
package runners
import utils.{ Constantes, ReversibleSparseBitSet2 }
import oscar.algo.Inconsistency
import oscar.algo.reversible._
import oscar.cp._
import oscar.cp.core.CPPropagStrength
import java.util.ArrayList
import java.util.Collection

import java.io.File
import java.io._

import Constraints._
import utils.Constantes
import oscar.cp._

import scala.io.Source

//Reified model


object BasicM extends CPModel with App {

  //parser
  val parser = argsParser()

  //Begin
  val mySolver = this.solver
  val t1 = System.nanoTime
  parser.parse(args, Config()) match {
    case Some(config) =>
      System.err.println("Uploading " + config.tdbFile.getName + "....")

      val fileLines = Source.fromFile(config.tdbFile.getAbsolutePath).getLines.filter(!_.trim().isEmpty)

      val tdbHorizontal: Array[Array[Int]] = fileLines.map { line => line.trim().mkString.split("\\s+").map(_.toInt) }.toArray
      val max: Int = tdbHorizontal.map(_.max).max
      val tdbVertical: Array[Set[Int]] = Array.fill(max + 1)(Set[Int]())

      for (i <- tdbHorizontal.indices) {
        for (v <- tdbHorizontal(i)) {
          tdbVertical(v) += i
        }
      }
      val nTrans = tdbHorizontal.length
      val nItems = max + 1

      var frequency = config.minsup.toInt

      if (config.minsup <= 1) frequency = (config.minsup * nTrans).ceil.toInt //--//NOTICE:ceil integer is taken for the support

      val duration = (System.nanoTime - t1) / 1e9d
      println("Data upload time = " + (duration - (duration % 0.001)) + "s")

      //Compute MISs
      var mis = new Array[Int](nItems)
      val beta = config.beta
      val ls = frequency
      var j = nItems

      while (j > 0) {
        j -= 1
        val inter = beta.toFloat * tdbVertical(j).size.toFloat
        if (inter > ls) {
          mis(j) = Math.round(inter)
        } else {
          mis(j) = ls
        }
      }

      //Put MISs in File
      if (config.mis) {
        var fw = new FileWriter("MIS_" + config.tdbFile.getName + "_" + ls + "_" + beta, false)
        while (j > 0) {
          j -= 1
          fw.write(j + " " + mis(j) + "\n")
        }

        fw.close()
      }

      //MISs statistics
      var avg = 0.0
      var n = 0
      for (i <- 0 until nItems) {
        if (tdbVertical(i).size != 0) {
          avg = avg + mis(i)
          n = n + 1
        }
      }

      System.err.println("Dataset: " + config.tdbFile.getName)
      System.err.println("support:" + frequency + " Beta:" + beta + " nTrans:" + nTrans + " nItems:" + n + " MIS-Min:" + mis.min + " MIS-Max:" + mis.max + " MIS-Avg:" + (avg / n).toInt)

      //File to Put results
      val gw = new FileWriter("MISresults", true)
      val freq = frequency.toDouble * 100 / nTrans.toDouble
      gw.write("Mining Frequent Itemsets on " + config.tdbFile.getName + "--> LS: " + frequency + "(" + BigDecimal(freq).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble + "%" + ") Beta: " + beta + "  nTrans: " + nTrans + " nItems: " + n + " MIS-Min:" + mis.min + " MIS-Max:" + mis.max + " MIS-Avg:" + (avg / n).toInt + "\n")

      val t2 = System.nanoTime
      //Modelling the problem
      System.err.println("Modelling the problem into CP...")

      //Declare variables
      val I = Array.fill(nItems)(CPBoolVar())
      val T = Array.fill(nTrans)(CPBoolVar())
      val S = CPIntVar(mis)

      //Reified model
      val D: Array[Array[Int]] = Array.fill(nTrans)(Array.fill(nItems)(0))
      for (j <- 0 until nTrans) {
        for (i <- tdbHorizontal(j)) {
          D(j)(i) = 1
        }
      }

      for (j <- 0 until nTrans) {
        mySolver.add((T(j) ==> (sum(0 until nItems)(i => I(i) * (1 -  D(j)(i))) ?=== 0)) && ((sum(0 until nItems)(i => I(i) * (1 -  D(j)(i))) ?=== 0) ==> T(j)))

      }
      //mySolver.add(sum(0 until nTrans)(i => T(i)) >= 20)
      //Or

      val maxmis = mis.max
      mySolver.add(S - maxmis >= minimum(0 until nItems)(k => I(k)*(mis(k) - maxmis)))

      for (i <- 0 until nItems) {
        mySolver.add(I(i) ==> (sum(0 until nTrans)(j => T(j)*D(j)(i)) ?>= S))
      }
      
      val modelizationTime = (System.nanoTime - t2) / 1e9d
      val modtime = (modelizationTime - (modelizationTime % 0.001))

      println("Modelization time = " + modtime + "s")

      val Isorted = (0 until I.size).sortBy(mis(_)).map(I(_)).toArray
      //val Isorted =  I

      //Verbose details if true
      if (config.verbose) {
        val letters = 0 to nItems
        mySolver.onSolution {
          print(">\t")
          var i = 0
          while (i < I.length) {
            if (I(i).min == 1)
              print(letters(i) + "(" + mis(i) + "),")
            i += 1
          }
          print(" S = " + S)
          println("\t:\t" + Constantes.curSupport)

        }
      }

      System.err.println("Start the Mining...")

      //Strategy of search
      mySolver.search {
        binaryStatic(Isorted, _.max)
      }

      //The timeout = 3600s (default)
      var timeout = config.timeout.toInt

      //Start the solving
      val stats = mySolver.start(timeLimit = timeout)

      val mb = 1024 * 1024
      val runtime = Runtime.getRuntime
      val usedMemory = (runtime.totalMemory - runtime.freeMemory) / mb
      
      //Print results
      println("Solving Time(s) = " + stats.time / 1000.0 + "s" + "\t" + " Total Time(s) = " + ((stats.time / 1000.0 + duration + modelizationTime) - ((stats.time / 1000.0 + duration + modelizationTime) % 0.001)) + "s " + "\t" + "Solutions = " + stats.nSols + "\t" + "Nodes = " + stats.nNodes + "\t" + "Fails = " + (stats.nFails - stats.nSols) + "\t" + "Used Memory = " + usedMemory + " MB")
      gw.write("Data upload time = " + (duration - (duration % 0.001)) + "s ModelisationTime = " + modtime + "s Solving Time = " + stats.time / 1000.0 + "s Total Time(s) = " + ((stats.time / 1000.0 + duration + modelizationTime) - ((stats.time / 1000.0 + duration + modelizationTime) % 0.001)) + " Solutions = " + stats.nSols + " Nodes = " + stats.nNodes + " Fails = " + (stats.nFails - stats.nSols) + " Used Memory=  " + (runtime.totalMemory - runtime.freeMemory) / mb + " MB" + "\n")
      gw.write("      -----------------------------------------------------------------     \n")
      gw.close()

      if (config.csv) {
        //Put results in an CSV file
        val r = new FileWriter("MISresultsCSV.csv", true)
        r.write(config.tdbFile.getName +
          ";" + frequency + ";"
          + BigDecimal(freq).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
          + "%" + ";" + beta + ";" + nTrans + ";" + n + ";" + mis.min + ";" + mis.max + ";"
          + (avg / n).toInt + ";" + stats.time / 1000.0
          + ";" + ((stats.time / 1000.0 + duration + modelizationTime) - ((stats.time / 1000.0 + duration + modelizationTime) % 0.001))
          + ";" + stats.nSols + ";" + stats.nNodes + ";" + (stats.nFails - stats.nSols) + ";" + usedMemory + "\n")

        r.close()
      }

      System.err.println("...done " + this.getClass.getName)

  }

  def printHead(): Unit = {
    System.err.println(
      """
    /** CP4MIS for mining frequent itemsets with multiple minimum supports (OscaR Solver) v1.0
    */
      """)
  }

  def argsParser(): scopt.OptionParser[Config] = {
    new scopt.OptionParser[Config]("CP4MIS") {
      head("CP4MIS", "1.0 (since March 2021)")

      arg[File]("<TDB File>") action { (x, c) =>
        c.copy(tdbFile = x)
      } validate { x =>
        if (x.exists()) success else failure("<TDB File> does not exist")
      } text ("the input transactions database")
      arg[Double]("<Lsup>") action { (x, c) =>
        c.copy(minsup = x)
      } validate { x =>
        if (x > 0) success else failure("Value <Lsup> must be > 0")
      } text ("the lower support - the lower bound of the frequency (MIS-min) - represents the minimum number of transactions. If <lsup> is between 0 and 1 it the relative support and the absolute support otherwise")
      arg[Double]("<Beta>") action { (x, c) =>
        c.copy(beta = x)
      } validate { x =>
        if (x >= 0 && x <= 1) success else failure("Value <Beta> must be Between 0 and 1")
      } text ("Beta Value")
      opt[Int]('D', "Distance") optional () valueName ("<Distance-UB>") action { (x, c) =>
        c.copy(distance = true, uperbound = x)
      } text ("Distance constraint: the upper bound of the distance between MISs in the itemset")
      opt[Int]('C', "Cardinality") optional () valueName ("<Cardinality-LB>") action { (x, c) =>
        c.copy(cardinality = true, lowerbound = x)
      } text ("Cardinality constraint: the lower bound size of the itemset")
      opt[Int]('K', "K-pattern") optional () valueName ("<K>") action { (x, c) =>
        c.copy(kpattern = true, k = x)
      } text ("K pattern mining: mine K distinct itemsets")
      opt[Unit]("verbose") abbr ("v") action { (_, c) =>
        c.copy(verbose = true)
      } text ("output all result with every details")
      opt[Unit]("mis-values") abbr ("mis") action { (_, c) =>
        c.copy(mis = true)
      } text ("Store the MIS values in a file 'MIS-TDBName-MISmin-beta'")
      opt[Unit]("CSV") abbr ("csv") action { (_, c) =>
        c.copy(csv = true)
      } text ("Put results in a CSV file")
      opt[Int]("timeout") abbr ("to") action { (x, c) =>
        c.copy(timeout = x)
      } text ("the timeout in seconds")

      help("help") text ("Usage of CP4MIS")

      override def showUsageOnError = true
    }
  }

}