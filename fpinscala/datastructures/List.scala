package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil         => 0
        case Cons(x, xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
        case Nil          => 1.0
        case Cons(0.0, _) => 0.0
        case Cons(x, xs)  => x * product(xs)
    }
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
        
    def tail[T](list: List[T]) : List[T] = list match {
        case Nil    => Nil
        case Cons(_, t) => t
    }
     
    def setHead[T](h: T, l: List[T]) : List[T] = l match {
        case Nil    => sys.error("cant replace head of empty list")
        case Cons(_, t)    => Cons(h, t)
    }
    
    def dropE[T](l: List[T], n: Int) : List[T] = 
        if (n == 0)
            l
        else
            dropE(tail(l), n - 1)
            
    def drop[T](l: List[T], n: Int) : List[T] =
        if (n == 0)
            l
        else l match {
            case Nil => Nil
            case Cons(_, t) => drop(t, n - 1)
        }
    
    def take[T](l: List[T], n: Int) : List[T] = 
        if (n == 0 )
            Nil
        else l match {
            case Nil => Nil
            case Cons(h, t) => Cons(h, take(t, n - 1))
        }
    
    def dropWhile[T](l: List[T], p: T => Boolean) : List[T] = l match {
        case Cons(h, t) if (p(h)) => dropWhile(t, p)
        case _ => l
    }
    
    def init[T](l: List[T]) : List[T] = l match {
        case Nil => sys.error("empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
    }
    
    def takeWhile[T](l: List[T])(p: T => Boolean) : List[T] = l match {
        case Nil => Nil
        case Cons(h, t) => if (p(h)) 
                                Cons(h, takeWhile(t)(p))
                           else takeWhile(t)(p) 
    }
    
    def length(l: List[Any]) : Int = l match {
        case Nil => 0
        case Cons(_, xs) => 1 + length(xs)
    }
    
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B) : B = as match {
        case Nil => z
        case Cons(a, as) => foldLeft(as, f(z, a))(f)
    }
    
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
    
    def reverse[T](ts: List[T]) : List[T] = foldLeft(ts, Nil: List[T])((b, a) => Cons(a, b))
    
    def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B) : B = foldRight(reverse(as), z)((a:A, b:B) => f(b, a))
    
    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B) : B = foldLeft(reverse(as), z)((b:B, a:A) => f(a, b))
    
    def length2(l: List[Any]) : Int = foldLeft(l, 0)((b, _) => b + 1)
    
    def append[T](l: List[T], l2: List[T]) : List[T] = foldRight(l, l2)((a, b) => Cons(a, b))
    
    def concat[T](l: List[List[T]]) : List[T] = foldRight(l, Nil:List[T])((sublist, result) => foldRight(sublist, result)((elem, res) => Cons(elem, res)))
    
    def map[A,B](as: List[A])(f: A => B): List[B] = as match {
        case Nil => Nil
        case Cons(h, t) => Cons(f(h), map(t)(f))
    }
    
    def map2[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((a,b) => Cons(f(a), b))
        
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
        case Nil => Nil
        case Cons(h, t) => append(f(h), flatMap(t)(f))
    }
    
    def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
        case Nil => Nil
        case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f) 
    }
    
    def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)
    
    def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C) : List[C] = (as, bs) match {
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
    
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
        case (Nil, Nil) => true
        case (Nil, _) => false
        case (_, Nil) => true
        case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2 
                && (take(t1, length(t2)) == t2)) 
                true 
        else hasSubsequence(t1, sub)
    }
}