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

  def sleepyPar(n: Int) = Par.delay {
    Thread.sleep(n * 1000)
    Par.unit(n)
  }

  def sleepy(n: Int) = {
    Thread.sleep(n * 1000)
    n
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

    // 7.4
    // asyncF impl
    def _lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def _asyncF[A,B](f: A => B): A => Par[B] = a => _lazyUnit(f(a))

    // 7.5
    def _sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight[Par[List[A]]](Par.unit(List())){(p: Par[A], acc: Par[List[A]]) =>
        Par.map2(p, acc)( _ :: _)
      }
    }

    // 7.6
    def _parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] =
        as map (_asyncF((a: A) => if(f(a)) List(a) else List()))
      map(_sequence(pars))(_.flatten)
    }

    // 7.7
    // revisit

    // 7.8
    // revisit

    // 7.9
    // revisit

    // 7.10
    // revisit

    // 7.11
    // choiceN
    def _choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      es => choices(run(es)(n).get)(es)
    def _choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      _choiceN(Par.map(cond)( if (_) 0 else 1))(List(t, f))

    // 7.12
    // choiceMap
    def _choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => choices(run(es)(key).get)(es)

    // 7.13
    // chooser
    def _chooser[A,B](a: Par[A])(choices: A => Par[B]): Par[B] =
      es => choices(run(es)(a).get) (es)
    def __choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      _chooser(cond)(if (_) t else f)
    def __choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
      _chooser(n)(choices)
    def __choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      _chooser(key)(choices)

    // 7.14
    // join
    def _join[A](aa: Par[Par[A]]): Par[A] =
      es => run(es)(run(es)(aa).get())
    def _joinUsingBind[A](aa: Par[Par[A]]): Par[A] =
      _chooser(aa){a => a}
    def _flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] =
      _join(Par.map(a)(f))

    println("\nTESTING.....")
    using(Executors.newFixedThreadPool(8)) {es => {
      val p000 = _map2(Par.fork(sleepyPar(1)), Par.fork(sleepyPar(2)))(_ + _)
      println("Waiting on P000 ... ")
      println(s"Got (waiting): ${p000(es).get}")
    }}

    println("\nTESTING.....")
    using(Executors.newFixedThreadPool(8)) {es => {
      val p001 = _map2(Par.fork(sleepyPar(1)), Par.fork(sleepyPar(4)))(_ + _)
      println("Waiting on P001 ... ")
      try {
        println(s"Should not get this result: ${p001(es).get(3000, TimeUnit.MILLISECONDS)}")
      } catch {
        case e: Exception => println("P (properly) timed out...")
      }
    }}

    println("\nTESTING2.....")
    using(Executors.newFixedThreadPool(8)) {es => {
      val p002 = _lazyUnit(sleepy(6))
      println("Waiting on P002 (_lazyUnit) ... ")
      println(s"Got (waiting _lazyUnit): ${p002(es).get}")
    }}

    println("\nDEADLOCK?...")
    using(Executors.newFixedThreadPool(2)) {ftp => {
      val a = _lazyUnit(42 + 1)
      println(Par.equal(ftp)(a, fork(a)))
    }}

    println("\nFILTER...")
    using(Executors.newFixedThreadPool(2)) {ftp => {
      val l = List("", "1", "02", "003", "4", "05", "006")
      val pl = _parFilter(l)(_.length < 3)
      println(Par.equal(ftp)(unit(List("", "1", "02", "4", "05")), pl))
    }}

    println("\nChoiceN...")
    using(Executors.newFixedThreadPool(8)) {ftp => {
      val cs = List("a", "b", "c").map(Par.unit)
      val res0 = _choiceN(sleepyPar(1))(cs)
      println(Par.equal(ftp)(Par.unit("b"), res0))
      val res1 = __choiceN(sleepyPar(1))(cs)
      println(Par.equal(ftp)(Par.unit("b"), res1))
    }}

    println("\nChoice...")
    using(Executors.newFixedThreadPool(8)) {ftp => {
      // val cs = List("a", "b", "c").map(Par.unit) //   (Par.unit("a)", "1", "02", "003", "4", "05", "006")
      val res0 = _choice(Par.unit(false))(sleepyPar(1), sleepyPar(0))
      println(Par.equal(ftp)(Par.unit(0), res0))
      val res1 = __choice(Par.unit(false))(sleepyPar(1), sleepyPar(0))
      println(Par.equal(ftp)(Par.unit(0), res1))
    }}

    println("\nChoiceMap...")
    using(Executors.newFixedThreadPool(8)) {ftp => {
      // val cs = List("a", "b", "c").map(Par.unit) //   (Par.unit("a)", "1", "02", "003", "4", "05", "006")
      val res0 = _choiceMap(Par.unit("b"))(Map("a" -> sleepyPar(1), "b" -> sleepyPar(2), "c" -> sleepyPar(3)))
      println(Par.equal(ftp)(Par.unit(2), res0))
      val res1 = _choiceMap(Par.unit("b"))(Map("a" -> sleepyPar(1), "b" -> sleepyPar(2), "c" -> sleepyPar(3)))
      println(Par.equal(ftp)(Par.unit(2), res1))
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

  }
}
