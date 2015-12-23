object MyModuleBis {
	sealed trait Tree[+A]
	case class Leaf[A](value: A) extends Tree[A]
	case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

	def size(t: Tree[String]): Int = t match {			
		case Leaf(value)	=> 1
		case Branch(left, right) => 1 + size(left) + size(right)
	}

	def maximum(t: Tree[Int]): Int = t match {
		case Leaf(value) => value
		case Branch(left, right) => maximum(left) max maximum(right)
	}

	def depth(t: Tree[Int]): Int = t match {
		case Leaf(value) => 1
		case Branch(left, right) => 1 + depth(left) max depth(right)
	}

	def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
		case Leaf(value) => Leaf(f(value))
		case Branch(left, right) => Branch(map(left)(f), map(right)(f))
	}

	def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
		case Leaf(value) => f(value)
		case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
	}

	def fSize(t: Tree[String]): Int = {
		fold(t)(String => 1)((x:Int,y:Int) => 1 + x + y )
	}

	def fMaximum(t: Tree[Int]): Int = {
		fold(t)((v: Int) => v)((x:Int, y:Int) => x.max(y))
	}

	def fDepth(t: Tree[Int]): Int = {
		fold(t)(Int => 1)((x:Int, y:Int) => 1 + x.max(y))
	}

	def fMap[A,B](t: Tree[A])(f: A => B): Tree[B] = {
		fold (t) ((v: A) => Leaf(f(v)): Tree[B]) ((x:Tree[B], y:Tree[B]) => Branch(x , y))
	}

	def main(args: Array[String]): Unit = {
		println(size(Branch(Branch(Leaf("a"), Leaf("b")),Branch(Leaf("c"), Leaf("d")))))
		println(fSize(Branch(Branch(Leaf("a"), Leaf("b")),Branch(Leaf("c"), Leaf("d")))))

		println(maximum(Branch(Branch(Leaf(1), Leaf(2)),Branch(Leaf(3), Leaf(4)))))
		println(fMaximum(Branch(Branch(Leaf(1), Leaf(2)),Branch(Leaf(3), Leaf(4)))))
		
		println(depth(Branch(Branch(Leaf(1), Leaf(2)),Branch(Leaf(3), Leaf(4)))))
		println(fDepth(Branch(Branch(Leaf(1), Leaf(2)),Branch(Leaf(3), Leaf(4)))))
		
		println(map(Branch(Branch(Leaf(1), Leaf(2)),Branch(Leaf(3), Leaf(4))))(_*2))
		println(fMap(Branch(Branch(Leaf(1), Leaf(2)),Branch(Leaf(3), Leaf(4))))(_*2))
	}
}
