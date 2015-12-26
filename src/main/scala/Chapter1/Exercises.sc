def factorial(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, acc: Int): Int = {
    if (n==1) acc
    else go(n-1, n*acc)
  }
  go(n,1)
}
factorial(3)
factorial(4)
factorial(5)
def fib(n: Int): Int = {
  @annotation.tailrec
  def go(n:Int, n1: Int, n2: Int): Int = {
    if (n==1) n1
    else go(n-1, n1+n2, n1)
  }
  if (n==0) 1
  else go(n, 1, 1)
}
fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)

