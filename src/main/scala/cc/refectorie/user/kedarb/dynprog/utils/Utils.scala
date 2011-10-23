package cc.refectorie.user.kedarb.dynprog.utils

import java.util.Random
import collection.mutable.ArrayBuffer
import cc.refectorie.user.kedarb.dynprog.types.FtrVec
import scala.util.Sorting

object Utils {
  def stopWatch(name: String, puts: String => Any = println(_))(f: => Any) = {
    val startTime = System.currentTimeMillis
    f
    var totalTime = 1.0 * (System.currentTimeMillis - startTime)
    totalTime /= 1000
    puts("%s took %.2f s".format(name, totalTime))
  }

  def fail(obj: Any) = new RuntimeException(obj.toString)

  def TODO = new RuntimeException("TODO: implement")

  def FUTURE = new RuntimeException("FUTURE: implement")

  // Faster alternatives to for (i <- 0 until n)
  def forIndex(n: Int, f: Int => Any): Unit = {
    var i = 0
    while (i < n) {f(i); i += 1}
  }

  def forIndex(a: Int, b: Int, f: Int => Any): Unit = {
    var i = a
    while (i < b) {f(i); i += 1}
  }

  def forReverseIndex(n: Int, f: Int => Any): Unit = {
    var i = n - 1
    while (i >= 0) {f(i); i -= 1}
  }

  def forReverseIndex(b: Int, a: Int, f: Int => Any): Unit = {
    var i = b - 1
    while (i >= a) {f(i); i -= 1}
  }

  def forAllIndex(n: Int, f: Int => Boolean): Boolean = {
    var i = 0
    while (i < n) {if (!f(i)) return false; i += 1}
    true
  }

  def existsIndex(n: Int, f: Int => Boolean): Boolean = {
    var i = 0
    while (i < n) {if (f(i)) return true; i += 1}
    false
  }

  // iterate over array with index
  def foreachIndex[@specialized A](array: Array[A], f: (Int, A) => Any): Unit = {
    var i = 0
    while (i < array.length) {f(i, array(i)); i += 1}
  }

  def foreachSortedIndex(array: Array[Double], f: (Int, Double) => Any): Unit = {
    var i = -1
    val idxarray = array.toSeq.map(x => {
      i += 1
      (i, x)
    })
    Sorting.stableSort(idxarray, (a: (Int, Double), b: (Int, Double)) => a._2 > b._2).foreach {
      t => f(t._1, t._2)
    }
  }

  // map helpers
  def mapIndex[@specialized A: ClassManifest](n: Int, f: Int => A): Array[A] = {
    val result = new Array[A](n)
    var i = 0
    while (i < n) {result(i) = f(i); i += 1}
    result
  }

  def mapIndex[@specialized A: ClassManifest](a: Int, b: Int, f: Int => A): Array[A] = mapIndex(a, b, 1, f)

  def mapIndex[@specialized A: ClassManifest](a: Int, b: Int, incr: Int, f: Int => A): Array[A] = {
    val result = new Array[A]((b - a + incr - 1) / incr)
    var i = 0
    var x = a
    while (x < b) {result(i) = f(x); i += 1; x += incr}
    result
  }

  // shuffling helpers
  def shuffle[T](arr: Array[T]): Array[T] = shuffle(new Random, arr)

  def shuffle[T](arr: ArrayBuffer[T]): ArrayBuffer[T] = shuffle(new Random, arr)

  def shuffle[T](rnd: Random, arr: Array[T]): Array[T] = {
    for (n <- Iterator.range(arr.length - 1, 0, -1)) {
      val k = rnd.nextInt(n + 1)
      val t = arr(k); arr(k) = arr(n); arr(n) = t
    }
    arr
  }

  def shuffle[T](rnd: Random, arr: ArrayBuffer[T]): ArrayBuffer[T] = {
    for (n <- Iterator.range(arr.length - 1, 0, -1)) {
      val k = rnd.nextInt(n + 1)
      val t = arr(k); arr(k) = arr(n); arr(n) = t
    }
    arr
  }

  // create array methods
  // Creates an array buffer to collect the items,
  // and then creates an array just large enough to store it
  // doAll is passed a function that sticks stuff in the buffer
  def createArray[@specialized A: ClassManifest](doAll: (A => Any) => Any) = {
    val buf = new ArrayBuffer[A]
    doAll({x => buf.append(x)})
    buf.toArray
  }

  def createArrayFromIterator[@specialized A: ClassManifest](iter: Iterator[A]) = {
    val buf = new ArrayBuffer[A]
    for (a <- iter) buf.append(a)
    buf.toArray
  }

  //// Formatting: warning: these functions can be slow
  def fmt(x: Boolean): String = x.toString

  def fmt(x: Int): String = x.toString

  def fmt(x: Double): String = "%.3f".format(x)

  def fmt(x: Any): String = x match {
    case a: Boolean => fmt(a)
    case a: Double => fmt(a)
    case a: Int => fmt(a)
    case _ => x.toString
  }

  // parallel
  def parallel_foreach[A](numThreads: Int, a: Array[A], f: (Int, A, Boolean) => Any) = {
    if (a.size == 1) f(0, a.head, true) // Special case: no need to start threads
    else {
      import java.util.concurrent.{Executors, TimeUnit}
      val executor = Executors.newFixedThreadPool(numThreads)
      var exception: Option[Throwable] = None
      var primaryThread: Option[Thread] = None
      foreachIndex(a, {
        (i: Int, x: A) =>
          executor.execute(new Runnable() {
            def run = {
              try {
                if (exception == None) {
                  executor.synchronized {
                    if (primaryThread == None)
                      primaryThread = Some(Thread.currentThread)
                  }
                  f(i, x, primaryThread == Some(Thread.currentThread))
                }
              } catch {
                case t: Throwable =>
                  exception = Some(t); // Save exception
              }
            }
          })
      })
      executor.shutdown
      try {
        while (!executor.awaitTermination(10, TimeUnit.SECONDS)) {}
      } catch {case e: InterruptedException => throw fail("Interrupted")}
      exception match {case Some(t) => throw new RuntimeException(t); case None =>}
      executor.shutdownNow
    }
  }

  def parallel_iter_foreach[A](numThreads: Int, a: Iterator[A], f: (Int, A, Boolean) => Any) = {
    import java.util.concurrent.{Executors, TimeUnit}
    val executor = Executors.newFixedThreadPool(numThreads)
    var exception: Option[Throwable] = None
    var primaryThread: Option[Thread] = None
    var i = 0
    a.foreach{
      x: A =>
        executor.execute(new Runnable() {
          def run = {
            try {
              if (exception == None) {
                executor.synchronized{
                  if (primaryThread == None)
                    primaryThread = Some(Thread.currentThread)
                }
                f(i, x, primaryThread == Some(Thread.currentThread))
              }
            } catch {
              case t: Throwable =>
                exception = Some(t); // Save exception
            }
          }
        })
        i += 1
    }
    executor.shutdown
    try {
      while (!executor.awaitTermination(10, TimeUnit.SECONDS)) {}
    } catch {
      case e: InterruptedException => throw fail("Interrupted")
    }
    exception match {
      case Some(t) => throw new RuntimeException(t); case None =>
    }
    executor.shutdownNow
  }

  ////////////////////////////////////////////////////////////
  // Simple readable serialization/deserialization (not optimized for efficiency)
  def serializeArray[@specialized A: ClassManifest](puts: (String => Any), a: Array[A]) = puts(a.mkString(" "))

  def deserializeDoubleArray(gets: => String) = gets.split(" ").map(_.toDouble)

  def serializeArray2[@specialized A: ClassManifest](puts: (String => Any), a: Array[Array[A]]) = {
    puts(fmt(a.length))
    a.foreach(serializeArray(puts, _))
  }

  def deserializeDoubleArray2(gets: => String) = {
    val n = gets.toInt
    mapIndex(n, {_: Int => deserializeDoubleArray(gets)})
  }

  def serializeArray3[@specialized A: ClassManifest](puts: (String => Any), a: Array[Array[Array[A]]]) = {
    puts(fmt(a.length))
    a.foreach(serializeArray2(puts, _))
  }

  def deserializeDoubleArray3(gets: => String) = {
    val n = gets.toInt
    mapIndex(n, {_: Int => deserializeDoubleArray2(gets)})
  }

  def serializeFtrVec(puts: (String => Any), fv: FtrVec) = fv.serialize(puts)

  def deserializeFtrVec(gets: => String): FtrVec = (new FtrVec).deserialize(gets)
}
