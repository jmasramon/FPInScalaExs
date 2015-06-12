

object MyModule {
	def nFibo(n:Int):Int = {
	  n match {
		  case 0 => 0
		  case 1 => 1
		  case x => nFibo(x-1) + nFibo(x-2)
	  }
	}

	def factorial(n:Int): Int = {
		@annotation.tailrec
		def go(n: Int, acc:Int): Int = {
			if ( n == 0 ) acc
			else go(n-1, n*acc)
		}
		go(n,1)
	}

	def abs(n:Int): Int = {
		if ( n < 0 ) -n
		else n
	}

	def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
		def go(as:Array[A], acc:Boolean): Boolean = {
			if (as.length <= 1) acc
			else go(as.tail, ordered(as(0),as(1)) && acc )
		}
		go(as, true)
	}

	def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
		f(a,_)
	}

	def partial2[A,B,C](a: A, f: (A,B) => C): B => C = {
		b => f(a,b)
	}

	def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
		a => f(a,_)
	}

	def curry2[A,B,C](f: (A, B) => C): A => (B => C) = {
		a => b => f(a,b)
	}

	def uncurry[A,B,C](f: A => B => C): (A, B) => C ={
		(a,b) => f(a)(b)
	}

	def compose[A,B,C](f: B => C, g: A => B): A => C = {
		a => f(g(a))
	}

	private def formatAbs(x: Int) = {
		val msg = "The absolute value of %d is %d"
		msg.format(x, abs(x))
	}

	private def formatFactorial(n: Int) = {
		val msg = "The factorial of %d is %d."
		msg.format(n, factorial(n))
	}

	private def format(f: Int =>Int, x: Int ) = {
		val msg = "The result of applying f to %d is %d."
		msg.format(x, f(x))
	}

	sealed trait MyList[+A]
	case object Nil extends MyList[Nothing]
	case class Cons[+A](head:A, tail: MyList[A]) extends MyList[A]

	object MyList {
		def sum(ints: MyList[Int]): Int = ints match {
			case Nil => 0
			case Cons(x,xs) => x + sum(xs)
		}

		def product(ds: MyList[Double]): Double = ds match {
			case Nil => 1.0
			case Cons(0.0, _) => 0.0
			case Cons(x,xs) => x * product(xs)
		}

		def operateOnLists[A](ls: MyList[A])(operation: (A, A) => A)(identityValue: A ): A = ls match {
			case Nil => identityValue
			case Cons(x, xs) => operation(x,operateOnLists(xs)(operation)(identityValue))
		}

		//@annotation.tailrec
		def foldRight[A,B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
			case Nil => z
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))
		}

		def length[A](as: MyList[A]): Int = {
			foldRight(as, 0)( (x,y) => 1+y )
		}

		def foldLeft[A,B](as: MyList[A], z: B)(f: (B, A) => B): B = {
			@annotation.tailrec
			def go(as:MyList[A], acc:B): B = as match {
				case Nil => acc
				case Cons(x, xs) => go(xs, f(acc,x))
			}
			go(as, z)
		}

		// def foldLeftBis[A,B](as: MyList[A], z: B)(f: (B, A) => B): B = {
		// 	foldRight(as, z)( (x,y) => )
		// }

		def	reverse[A](as: MyList[A]): MyList[A] = {
			def go(as: MyList[A], acc: MyList[A]): MyList[A]  = as match {
				case Nil => acc
				case Cons(x,xs) => go(xs, setHead(acc,x))
			}
			go(as, Nil)
		}

		def sumBis(ints: MyList[Int]): Int ={
			foldLeft(ints, 0)((x,y) => y+x)
		}

		def productBis(ds: MyList[Double]): Double ={
			foldLeft(ds, 1.0)((x,y) => y*x)
		}

		def lengthBis[A](as: MyList[A]): Int = {
			foldLeft(as, 0)( (x,y) => 1+x )
		}

		def apply[A](as: A*): MyList[A] = {
			if (as.isEmpty) Nil
			else Cons(as.head, apply(as.tail: _*))
		}

		def head[A](as: MyList[A]): A = as match {
			// case Nil => Nothing
			case Cons(x,xs) => x
		}

		def tail[A](as: MyList[A]): MyList[A] = as match {
			case Nil => Nil
			case Cons(x,xs) => xs
		}

		def setHead[A](as: MyList[A], a: A): MyList[A] = as match {
			case Nil => Cons(a, Nil)
			case Cons(x,xs) =>  Cons(a, as)
		}

		def drop[A](l: MyList[A], n: Int): MyList[A] = l match {
			case Nil => Nil
			case Cons(x,xs) => {
				if (n>0) drop[A](xs, n-1)
				else l
			}
		}

		def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
			case Nil => Nil
			case Cons(x,xs) => {
				if (f(x)) dropWhile(xs,f)
				else l
			}
		}

		def dropWhileBis[A](l: MyList[A])(f: A => Boolean): MyList[A] = l match {
			case Nil => Nil
			case Cons(x,xs) => {
				if (f(x)) dropWhileBis(xs)(f)
				else l
			}
		}

		def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
			case Nil => a2
			case Cons(x,xs) => Cons(x, append(xs, a2))
		}

		def init[A](l: MyList[A]): MyList[A] = {
			def go(l:MyList[A], acc:MyList[A]): MyList[A] = l match {
				case Nil => Nil
				case Cons(x,Nil) => acc
				case Cons(x,xs) => go(xs, Cons(x, acc))
			}
			go(l,Nil)
		}
	}


	def main(args: Array[String]): Unit = {
	  println(format(abs, -42))
	  println(format(factorial, 7))
	  println("Array (1,2,3,4) is sorted? = " + isSorted(Array(1,2,3,4), (x:Int,y:Int) => x<y))
	  println("Array (a,b,c,d) is sorted? = " + isSorted(Array("a","c","b","d"), (x:String,y:String) => x<y))

	  println(MyList.drop(MyList(1,2,3,4,5),3))

	  println(MyList.dropWhile(MyList(1,2,3,4,5),(x: Int) => x<3))
	  println(MyList.dropWhileBis(MyList(1,2,3,4,5))( _<3 ))

	  println(MyList.append(MyList(1,2,3,4,5),MyList(6,7,8)))

	  println(MyList.init(MyList(1,2,3,4,5)))

	  println(MyList.product(MyList(1,2,3,4,5)))

	  println(MyList.operateOnLists[Double](MyList(1,2,3,4,5))(_*_)(1.0))

		println(MyList.length(MyList(1,2,3,4,5)))
	  println(MyList.length(MyList(1,2,3)))
	  println(MyList.length(Nil))

	  println(MyList.foldRight(MyList(1,2,3,4,5), 1.0)(_*_))
	  println(MyList.foldRight(MyList(1,2,3,4,5), Nil:MyList[Int])(Cons(_,_)))

	  println(MyList.foldLeft(MyList(1,2,3,4,5), 1.0)((x,y) => y*x))
	  println(MyList.foldLeft(MyList(1,2,3,4,5), Nil:MyList[Int])((x,y) => Cons(y,x)))

		assert (MyList.foldRight(MyList(1,2,3,4,5), 0.0)(_+_) == 15)
		assert (MyList.foldLeft(MyList(1,2,3,4,5), 0.0)(_+_) == 15)
		println("Fold Right: " + List(1,2,3,4,5).foldRight(0)(_-_))
		println (MyList.foldRight(MyList(1,2,3,4,5), 0.0)(_-_))
		assert (MyList.foldRight(MyList(1,2,3,4,5), 0.0)(_-_) == 3)
		println("Fold Left: " + List(1,2,3,4,5).foldLeft(0)(_-_))
		println(MyList.foldLeft(MyList(1,2,3,4,5), 0.0)(_-_))
		assert(MyList.foldLeft(MyList(1,2,3,4,5), 0.0)(_-_) == -15)


	  println(MyList.sumBis(MyList(1,2,3,4,5)))
	  println(MyList.productBis(MyList(1,2,3,4,5)))

	  println(MyList.lengthBis(MyList(1,2,3,4,5)))
	  println(MyList.lengthBis(MyList(1,2,3)))
		println(MyList.lengthBis(Nil))

		println(MyList.reverse(MyList(1,2,3,4)))
	}
}
