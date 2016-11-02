def p(f: Int => Int): (Int => Int) = (n: Int) => if (n == 0) 1 else n * f(n - 1)

def Y[T](f: (T => T) => (T => T)): (T => T) = { x => f(Y(f))(x) }

def fact = Y(p)

fact(6)




