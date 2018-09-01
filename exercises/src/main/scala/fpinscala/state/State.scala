package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng nextInt match {
    case (Int.MinValue, newRng) => nonNegativeInt(newRng)
    case (v, newRng) => (v.abs, newRng)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case (Int.MaxValue, newRng) => double(newRng)
    case (v, newRng) => (v.toDouble/Int.MaxValue, newRng)
  }

  def _double: Rand[Double] = map(nonNegativeInt)(i => i/(Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, iRng) = rng.nextInt
    val (d, dRng) = double(iRng)
    ((i, d), dRng)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, dRng) = double(rng)
    val (i, iRng) = dRng.nextInt
    ((d, i), iRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  // TODO: make tail-recursive
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (List.empty, rng)
    else {
      val (v, rng1) = rng.nextInt
      val (vs, rng2) = ints(count - 1)(rng1)
      (v :: vs, rng2)
    }

  def _ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rnga) = ra(rng)
    val (b, rngb) = rb(rnga)
    (f(a, b), rngb)
  }

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a,b)))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs match {
      case Nil => (List.empty, rng)
      case r :: rs => {
        val (v, rng1) = r(rng)
        val (vs, rng2) = sequence(rs)(rng1)
        (v :: vs, rng2)
      }
    }
  }

  def _sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]())) ((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (v1, rng1) = f(rng)
    g(v1)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ i =>
    val mod = i % n
    if (i + (n -1) - mod >= 0)
      unit(mod)
    else
      nonNegativeLessThan(n)
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    }
  )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight (unit[S, List[A]](List()))  ((f, acc) => f.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- getS // Gets the current state and assigns it to `s`.
    _ <- setS(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def getS[S]: State[S, S] = State(s => (s, s))

  def setS[S](s: S): State[S, Unit] = State(_ => ((), s))

  type Rand[A] = State[RNG, A]
}

object Candy {
  import fpinscala.state.State._

//  def update = (i: Input) => (s: Machine) =>
//  def updateMachine = (input: Input) => (s: Machine) => {
  def updateMachine(input: Input)(s: Machine) = {
    (input, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose updateMachine))
    s <- getS
  } yield(s.coins, s.candies)
}

object StateTests {
  import fpinscala.state.Candy._

  def main(args: Array[String]): Unit = {
    println ("StateTests(machina!)...")
    val initialMachine  = Machine(true, 5, 10)
    val (finalState, finalMachine) = simulateMachine(List(Coin, Turn, Coin, Turn, Turn, Coin, Turn, Coin, Coin, Turn, Turn, Turn)).run(initialMachine)
//    val (finalState, finalMachine) = simulateMachine(List(Coin, Turn, Coin, Turn)).run(initialMachine)
    println(s"The final machine is: $finalMachine")
    println(s"The final state is: $finalState")
  }
}

object RngTests {
  import fpinscala.state.RNG._

  def main(args: Array[String]): Unit = {
    println("StateTests...")
    val rng = Simple(4)

    val (nextInt, nextRng) = rng.nextInt
    println(s"nextInt: ${nextInt}")

    val (nnInt, nnRng) = nonNegativeInt(nextRng)
    println(s"nonNegativeInt: ${nnInt}")

    val (d, doubleRng) = double(nnRng)
    println(s"double: ${d}")

    val (idVal, idRng) = intDouble(doubleRng)
    println(s"intDouble: ${idVal}")

    val (diVal, diRng) = doubleInt(idRng)
    println(s"doubleInt: ${diVal}")

    val (dddVal, dddRng) = double3(diRng)
    println(s"double3: ${dddVal}")

    val(intsVal, intsRng) = ints(5)(dddRng)
    println(s"ints: ${intsVal}")

    val (randIntVal, randIntRng) = int(intsRng)
    println(s"randInt: ${randIntVal}")

    val (unitVal, unitRng) = unit("hello")(randIntRng)
    println(s"unitVal: ${unitVal}")

    val (nneInt, nneRng) = nonNegativeEven(unitRng)
    println(s"nonNegativeEven: ${nneInt}")

    val (d2, double2Rng) = _double(nneRng)
    println(s"_double: ${d2}")

    val(_intsVal, _intsRng) = _ints(5)(double2Rng)
    println(s"_ints: ${_intsVal}")

    val (nnltInt, nnltRng) = nonNegativeLessThan(100)(_intsRng)
    println(s"nonNegativeLessThan: ${nnltInt}")
  }
}
