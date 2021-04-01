
package runners

import java.io.File
import java.io._

import Constraints._
import utils.Constantes
import oscar.cp._

import scala.io.Source

case class Config(

  tdbFile: File = new File("."),
  minsup: Double = 0.0,
  beta: Double = 1.0,
  uperbound: Int = 0,
  k: Int = 0,
  lowerbound: Int = 0,
  timeout: Int = 3600,
  strategy: String = "",
  outResultFile: File = new File("."),

  cardinality: Boolean = false,
  kpattern: Boolean = false,
  distance: Boolean = false,
  verbose: Boolean = false,
  mis: Boolean = false,
  csv: Boolean = false,

  supfile: File = new File("."))

object MISRunner extends CPModel with App {

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

      if (config.kpattern) {

        System.err.println("K-pattern mining with k = " + config.k + "\n")
        gw.write("K-pattern mining with k = " + config.k + "\n")
        //Declare variables
        val R = Array.fill(config.k)(Array.fill(nItems)(CPBoolVar()))
        val Ss = Array.fill(config.k)(CPIntVar(mis))

        //Add the Global Constraint (FrequentMIS)
        for (i <- 0 until config.k) {
          mySolver.add(new FrequentMIS(R(i), Ss(i), mis, nItems, nTrans, tdbVertical))
        }

        //Additional constraints
        // 1. Distinct items
        for (i <- 0 until config.k) {
          for (k <- i + 1 until config.k) {
            for (j <- 0 until nItems) {
              if (tdbVertical(j).size >= frequency)
                mySolver.add(R(i)(j).not || R(k)(j).not)
            }
          }
        }

        // Symmetry breaking
        for (i <- 0 until config.k - 1) {
          for (j <- 0 until nItems) {
            if (tdbVertical(j).size >= frequency)
              mySolver.add(R(i + 1)(j) <= sum(0 until j + 1)(l => R(i)(l)))
          }
        }

        // 2. Distance constraint
        if (config.distance) {
          for (k <- 0 until config.k) {
            for (i <- 0 until nItems) {
              if (tdbVertical(i).size >= frequency) {
                for (j <- i + 1 until nItems) {
                  if (tdbVertical(j).size >= frequency)
                    mySolver.add((R(k)(i) * R(k)(j) * (mis(i) - mis(j)).abs) <= config.uperbound)
                }
              }
            }
          }
          System.err.println("Upper bound of the distance: " + config.uperbound)
          gw.write("Upper bound of the distance: " + config.uperbound + "\n")
        }

        // 3.Cardinality constraint
        if (config.cardinality) {
          for (k <- 0 until config.k) {
            mySolver.add(sum(R(k)) >= config.lowerbound)
          }
          gw.write("Lower bound of the size: " + config.lowerbound + "\n")
          System.err.println("Lower bound of the size: " + config.lowerbound)
        }
        val modelizationTime = (System.nanoTime - t2) / 1e9d
        val modtime = (modelizationTime - (modelizationTime % 0.001))

        println("Modelization time = " + modtime + "s")

        var RSorted = (0 until R(0).size).sortBy(mis(_)).map(R(0)(_)).toArray
        //var RSorted = R(0)

        for (i <- 1 until config.k) {
          val r = (0 until R(i).size).sortBy(mis(_)).map(R(i)(_)).toArray
          //val r = R(i)
          RSorted = RSorted.clone() ++ r.clone()
        }

        if (config.verbose) {
          val letters = 0 to nItems
          mySolver.onSolution {

            var j = 0
            while (j < R.length) {
              print(">\t")
              var i = 0
              while (i < R(j).length) {
                if (R(j)(i).min == 1)
                  print(letters(i) + "(" + mis(i) + "),")
                i += 1
              }
              println(":\t" + Ss(j))
              j += 1
            }
            println("-------------")

          }
        }

        mySolver.search {
          binaryStatic(RSorted, _.min)
        }

        //val stats = mySolver.start(1)
        val stats = mySolver.start(timeLimit = 3600)

        System.err.println(stats)

        println("Solving Time(s) = " + stats.time / 1000.0 + "s" + "\t" + " Total Time(s) = " + ((stats.time / 1000.0 + duration + modelizationTime) - ((stats.time / 1000.0 + duration + modelizationTime) % 0.001)) + "s " + "\t" + "Solutions = " + stats.nSols + "\t" + "Nodes = " + stats.nNodes + "\t" + "Fails = " + (stats.nFails - stats.nSols))
        gw.write("Data upload time = " + (duration - (duration % 0.001)) + "s ModelisationTime = " + modtime + "s Solving Time = " + stats.time / 1000.0 + "s Total Time(s) = " + ((stats.time / 1000.0 + duration + modelizationTime) - ((stats.time / 1000.0 + duration + modelizationTime) % 0.001)) + " Solutions = " + stats.nSols + " Nodes = " + stats.nNodes + " Fails = " + (stats.nFails - stats.nSols) + "\n")
        gw.write("      -----------------------------------------------------------------     \n")
        gw.close()

        System.err.println("...done " + this.getClass.getName)

      } else {
        //Declare variables
        val I = Array.fill(nItems)(CPBoolVar())
        val S = CPIntVar(mis)

        //Add the Global Constraint (FrequentMIS)
        mySolver.add(new FrequentMIS(I, S, mis, nItems, nTrans, tdbVertical))

        //Additional constraints
        // 1. Distance constraint
        if (config.distance) {
          for (i <- 0 until nItems) {
            if (tdbVertical(i).size >= frequency) {
              for (j <- i + 1 until nItems) {
                if (tdbVertical(j).size >= frequency)
                  mySolver.add((I(i) * I(j) * (mis(i) - mis(j)).abs) <= config.uperbound)
              }
            }
          }
          System.err.println("Upper bound of the distance: " + config.uperbound)
          gw.write("Upper bound of the distance: " + config.uperbound + "\n")
        }

        // 2.Cardinality constraint
        if (config.cardinality) {
          mySolver.add(sum(I) >= config.lowerbound)
          gw.write("Lower bound of the size: " + config.lowerbound + "\n")
          System.err.println("Lower bound of the size: " + config.lowerbound)
        }
        val modelizationTime = (System.nanoTime - t2) / 1e9d
        val modtime = (modelizationTime - (modelizationTime % 0.001))

        println("Modelization time = " + modtime + "s")

        val Isorted = (0 until I.size).sortBy(mis(_)).map(I(_)).toArray
        //val Isorted =  I

        //Verbose details of true
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
          binaryStatic(Isorted, _.min)
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
        
        if(config.csv){
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