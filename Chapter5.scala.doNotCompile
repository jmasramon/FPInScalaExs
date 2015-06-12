object MyModule {
	sealed trait Stream[+A]{
		def toList: List[A] = this match {
			case Cons(h,t) => h() :: t().toList
			case _ => Nil 
		}
		
		def toListTR: List[A] = {
			@annotation.tailrec			
			def go(s:Stream[A], acc: List[A]): List[A] = s match {
				case Cons(h,t) => go(t(), h() :: acc)
				case _ => acc 
			}
			go(this, Nil).reverse
		}

		def take(n: Int): Stream[A] = this match {
			case Cons(h,t) => {
				if (n>0) Cons(h, () => t().take(n-1))
				else Empty
			}
			case _ => Empty
		}

		@annotation.tailrec
		final def drop(n: Int): Stream[A] = this match {
			case Cons(_,t) if (n>0) => t().drop(n-1)
			case _ => this
		}

		def takeWhile(p: A => Boolean): Stream[A] = this match {
			case Cons(h,t) if (p(h())) => Cons(h, () => t().takeWhile(p))
			case _ => Empty
		}
		
	  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
	    this match {
	      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
	      case _ => z
	    }

		def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

		def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b ) => p(a) && b)

		def takeWhileWithFoldR(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a,b) => if (p(a)) Cons(() => a, () => b) else Empty)

		// def headOption: Option[A] = foldRight[Option[A]](None)((a,_) => Some(a))

		def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](Empty)((a,b) => Cons(() => f(a), () => b))
		
		def filter(p: A => Boolean): Stream[A] = foldRight[Stream[A]](Empty)((a,b) => if (p(a)) Cons(() => a, () => b) else b)
		
		def append[B>:A](s: => Stream[B]): Stream[B] = foldRight[Stream[B]](s)((a,b) => Cons(() => a, () => b))

		def flatMap[B](f: A => Stream[B] ): Stream[B] = foldRight[Stream[B]](Empty)((a,b) => f(a).append(b))

		def map1[B](f: A => B): Stream[B] = unfold(this){ 
			case Cons(h,t) => Some(f(h()),t())
			case _ => None 
		}

		def take1(n: Int): Stream[A] = unfold((this,n)){ 
			case (Cons(h,t), 1) => Some(h(), (Stream.empty, 0))
			case (Cons(h,t), n) => Some(h(), (t(), n-1))
			case _ => None 
		}

		def takeWhile1(p: A => Boolean): Stream[A] = unfold(this){ 
			case Cons(h,t) if (p(h())) =>  Some(h(), t())  
			case _ => None
		}

		def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s2)){
			case (Cons(h,t), Cons(h2,t2))  => Some(f(h(), h2()), (t(),t2()))
			case _ => None
		}

		def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???
	
		override def toString(): String = this match {
			case Cons(h,t) => "(" + h() + ", " + t().toString() + ")"
			case _ => " Empty"
		}

	}

	case object Empty extends Stream[Nothing]
	case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]	
	
	object Stream {
		def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
			lazy val head = hd
			lazy val tail = tl
			Cons(() => head, () => tail)
		}
		
		def empty[A]: Stream[A] = Empty

		def apply[A](as: A*): Stream[A] =
			if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
		

	}

	def constant[A](a: A): Stream[A] = {
		Stream.cons(a, constant(a))
	}

	def from(n: Int): Stream[Int] = {
		Stream.cons(n, from(n+1))
	}

	def fibs(): Stream[Int] = {
		def go(f0:Int, f1:Int): Stream[Int] = Stream.cons(f0, go(f1, f0+f1))
		go(0,1)
	}

	sealed trait Option[+A]
	case class Some[+A](get: A) extends Option[A] 
	case object None extends Option[Nothing]

	def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match { 
		case Some((a,s)) => Stream.cons(a, unfold(s)(f))
		case None => Stream.empty
	}

	def fibs1(): Stream[Int] = unfold((0,1)){case(x,y) => Some(x+y,(y,x+y))}

	def from1(n: Int): Stream[Int] = unfold(n)(x => Some(n, n+1))

	def constant1[A](a: A): Stream[A] = unfold(a)(x => Some(x,x))

	def ones(): Stream[Int] = unfold(1)(_ => Some(1,1))


	def main(args: Array[String]): Unit = {
		println(Stream.cons( 1, Stream.cons( 2 , Stream.cons( 3, Stream.empty))))
		println(Stream(1,2,3).toList)
		println(Stream(1,2,3).toListTR)
		println(Stream(1,2,3).take(2))
		println(Stream(1,2,3).take(1))
		println(Stream(1,2,3).drop(1))
		println(Stream(1,2,3).drop(2))
		println(Stream(1,2,3).takeWhile(_ < 3))
		println(Stream(1,2,3).take(2).toList)
		println(Stream(1,2,3).exists(_ == 2))
		println(Stream(1,2,3).forAll(_ < 4))
		println(Stream(1,2,3).takeWhileWithFoldR(_ < 3))
		println(Stream(1,2,3).map(_ * 3))
		println(Stream(1,2,3).filter(_ != 2))
		println(Stream(1,2,3).append(Stream(4,5,6)))
		println(Stream(1,2,3).flatMap(x => Stream(x*2)))
		println(Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList)

		lazy val ones: Stream[Int] = Stream.cons(1, ones)
		println(ones.take(5).toList)
		println(ones.takeWhile(_ == 1).take(10).toList)
		println(constant(3).take(10).toList)
		println(from(3).take(10).toList)
		println(fibs().take(10).toList)

		println(Stream(1,2,3).take1(2).toList)
	}
}