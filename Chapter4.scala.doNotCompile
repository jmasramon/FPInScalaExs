object MyModule {
	sealed trait Option[+A] {
		def map[B](f: A => B): Option[B] = this match {
			case None => None
			case Some(get) => Some(f(get))
		}
	
		def flatMap[B](f: A => Option[B]): Option[B] = this match {
			case None => None
			case Some(get) => f(get)			
		}
	
		def getOrElse[B >: A](default: => B): B = this match {
			case None => default
			case Some(get) => get			
		}
	
		def orElse[B >: A](ob: => Option[B]): Option[B]  = this match {
			case None => ob
			case Some(get) => this			
		}
	
		def filter(f: A => Boolean): Option[A] = this match {
			case None => None
			case Some(get) => if (f(get)) this else None			
		}

	}

	case class Some[+A](get: A) extends Option[A] 
	case object None extends Option[Nothing]

	def varianceTry(xs: Seq[Double]): Seq[Option[Double]] = {
		val avg = (xs.sum/xs.length)
	  xs.map(Some(_).
	  				flatMap((x: Double) => Some(x - avg)).
	  				flatMap((x: Double) => Some(math.pow(x,2))))
	  	// .reduce()
	}

  def mean(xs: Seq[Double]): Option[Double] =
	  if (xs.isEmpty) None
	  else Some(xs.sum / xs.length)

	// def variance(xs: Seq[Double]): Seq[Option[Double]] = {
	// 	mean(xs) flatMap (m => mean(xs.map(x => math.pow(x-m,2))))
	// }

	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a match {
		case None => None
		case Some(valueA) => b match {
			case None => None
			case Some(valueB) => Some(f(valueA, valueB))
		}
	}

	def map3[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
		a flatMap (aa => b map (bb => f(aa, bb)))
	}

	def lift2[A,B,C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] = {
		map3(_,_)(f)
	}

	def sequence[A](a: List[Option[A]]): Option[List[A]] = {
		a.foldRight[Option[List[A]]](Some(Nil))((x,y) => map3(x,y)(_ :: _))
	}

	def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
		sequence ( a map f )
	}

	def traverseEfficient[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
		case Nil => Some(Nil)
		case x::xs => map3(f(x), traverseEfficient(xs)(f))(_::_)
	}

	def traverseEfficient2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
		a.foldRight[Option[List[B]]](Some(Nil))((x,y) => map3(f(x),y)(_::_))
	}


	sealed trait Either[+E, +A]{
		def map[B](f: A => B): Either[E, B] = this match {
			case Left(e) => Left(e)
			case Right(v) => Right(f(v)) 
		}

		def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
			case Left(e) => Left(e)
			case Right(v) => f(v) 
		}
		
		def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
			case Left(_) => b
			case Right(v) => Right(v) 
		} 
		
		def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
			case Left(e) => Left(e)
			case Right(va) => b match {
				case Left(e) => Left(e)
				case Right(vb) => Right(f(va, vb))
			} 
		}

		def map2Bis[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
			for {
				aa <- this
				bb <- b
			} yield (f(aa,bb))
		}
	}

	case class Left[+E](value: E) extends Either[E, Nothing] 
	case class Right[+A](value: A) extends Either[Nothing, A]

	def sequenceEither[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
		es.foldRight[Either[E,List[A]]](Right(Nil))((x,y) => x.map2(y)(_::_))
	}

	def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
		as.foldRight[Either[E,List[B]]](Right(Nil))((x,y) => f(x).map2(y)(_::_))
	}

	def sequenceEitherTrav[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
		traverseEither(es)(x => x) // A = Either[E,A] B=A
	}


	def main(args: Array[String]): Unit = {
		println(Some(3) map (_*2))
		
		println(Some(3) flatMap ((x:Int) => Some(x*2)))
		
		println(Some(3) getOrElse (2))
		println(None getOrElse (2))

		println(Some(3) orElse (Some(2)))
		println(None orElse (Some(2)))

		println(Some(3) filter (_ > 2))
		println(Some(3) filter (_ < 2))

		println(varianceTry(List(3.0, 4.0, 5.0)))
		println(mean(List(3.0, 4.0, 5.0)))
		// println(variance(List(3.0, 4.0, 5.0)))

		println(map2(Some(3), Some(4))(_+_))
		println(map3(Some(3), Some(4))(_+_))

		println(lift2((x:Int, y:Int) => x+y)(Some(3), Some(4)))
		
		println(sequence(List(Some(3), Some(4))))
		println(sequence(List(Some(3), Some(4), Some(5), None, Some(7))))
		
		println(traverse(List(3, 4, 5, 6, 7))((x:Int) => Some(x*2)))
		println(traverseEfficient(List(3, 4, 5, 6, 7))((x:Int) => Some(x*2)))
		println(traverseEfficient2(List(3, 4, 5, 6, 7))((x:Int) => Some(x*2)))
	}
}