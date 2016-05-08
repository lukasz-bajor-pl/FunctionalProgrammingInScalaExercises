Implement uncurry, which reverses the transformation of curry. Note that since => assocciates to the right, A => (b => C) can be written as a => B => C.

def uncurry[A,B,C](f: A => B => C): (A, B) => C