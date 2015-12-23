object MyOtherModule {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    ((if (n < 0) -(n + 1) else n), rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    ((n / (Int.MaxValue.toDouble + 1)), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng)
    ((i, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def go(acc: List[Int], count: Int, rng: RNG): (List[Int], RNG) = {
      if (count > 0) {
        val (i, rng2) = nonNegativeInt(rng)
        go(i :: acc, count - 1, rng2)
      } else {
        (acc, rng)
      }
    }
    go(Nil, count, rng)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  val positiveInt: Rand[Int] = nonNegativeInt
  val doub: Rand[Double] = double
  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => (f(s(rng)._1), s(rng)._2)
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(a => a - a % 2)

  def mappedDouble(rng: RNG): (Double, RNG) = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((a, b) => (a, b))

  val randIntDouble: Rand[(Int, Double)] = both(int, doub)
  val randDoubleInt: Rand[(Double, Int)] = both(doub, int)

  // def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => fs match {
  // 	case Cons(ra, fss) => {
  // 		val (a, rng2) = ra(rng)
  // 		(a::sequence(fss))
  // 	}
  // 	case Nil => 
  // }

  def main(args: Array[String]): Unit = {
    println(nonNegativeInt(new SimpleRNG(42)))
    println(double(new SimpleRNG(16159453)))
    println(intDouble(new SimpleRNG(16159453)))
    println(doubleInt(new SimpleRNG(16159453)))
    println(double3(new SimpleRNG(16159453)))
    println(ints(5)(new SimpleRNG(16159453)))
  }
}