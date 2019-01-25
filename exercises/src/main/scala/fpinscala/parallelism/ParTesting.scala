package fpinscala.parallelism

object ParTesting {

  import java.util.concurrent._
  import language.implicitConversions

  def using[A <: { def shutdown(): Unit }, B](param: A)(f: A => B): B =
    try {
      f(param)
    } finally {
      param.shutdown()
    }

  // works but not has some issues, see answer in answer key
  private case class MultiTimeoutFuture[A,B,C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {
    def isDone = a.isDone && b.isDone
    def get = {
      val ra = a.get
      val rb = b.get
      f(ra, rb)
    }

    def get(timeout: Long, units: TimeUnit) = {
      val toNanos = TimeUnit.NANOSECONDS.convert(timeout, units)
      val start = System.nanoTime
      val ra = a.get(toNanos, TimeUnit.NANOSECONDS)
      val elapsed = System.nanoTime - start
      val rb = b.get(toNanos - elapsed, TimeUnit.NANOSECONDS)
      f(ra, rb)
    }

    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
  }

  def thing(n: Int) = {
    Thread.sleep(n * 1000)
    Par.unit(n)
  }

  def main(args: Array[String]): Unit = {
    println ("ParTesting(machina!)...")

    parTest
    nbTest
  }

  def parTest: Unit = {
    import Par._

    // 7.1
    // def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = ???

    // 7.2
    // Par rep skipping

    // 7.3
    // timeout respecting map2
    def _map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        MultiTimeoutFuture(af, bf, f)
      }

    using(Executors.newFixedThreadPool(2)) {es => {
      val p000 = _map2(Par.fork(thing(1)), Par.fork(thing(2)))(_ + _)
      println("Waiting on P000 ... ")
      println(s"Got (waiting): ${p000(es).get}")
    }}

    using(Executors.newFixedThreadPool(4)) {es => {
      val p001 = _map2(Par.fork(thing(1)), Par.fork(thing(4)))(_ + _)
      println("Waiting on P001 ... ")
      try {
        println(s"Should not get this result: ${p001(es).get(3000, TimeUnit.MILLISECONDS)}")
      } catch {
        case e: Exception => println("P (properly) timed out...")
      }
    }}
  }

  def nbTest: Unit = {
    import Nonblocking._

    // 7.4
    // asyncF impl
    def _asyncF[A,B](f: A => B): A => Par[B] = a => Par.lazyUnit(f(a))

    // 7.5
    def _sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight[Par[List[A]]](Par.unit(List())){(p: Par[A], acc: Par[List[A]]) =>
        Par.map2(p, acc)( _ :: _)
      }
    }

    // 7.6
  }
}
