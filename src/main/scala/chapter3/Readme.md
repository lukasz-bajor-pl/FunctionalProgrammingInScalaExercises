Chapter 3
=========

Exercise 3.1
------------
What will be the result of the following match expression ?

	val x = List(1,2,3,4,5) match {
	    case Cons(x, Cons(2, Cons(4, _))) => x
	    case Nil => 42
	    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x+y
	    case _ => 101
	}

Exercise 3.2
------------
Implement the function `tail` for removing the first element of the `List`. Note that function takes constant time.
What are different choices you could make in your implementation if the `List` is `Nil`?

