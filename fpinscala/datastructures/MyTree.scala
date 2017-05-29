package fpinscala.datastructures

sealed trait MyTree[+T] { }

case class Leaf[A](a:A) extends MyTree[A] {}

case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A] {
    
}

object MyTree {
    def size(t: MyTree[Any]): Int = t match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }
    
    def maximum(t: MyTree[Int]): Int = t match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }
    
    def depth(t: MyTree[Any]): Int = t match {
        case Leaf(v) => 0
        case Branch(l, r) => (depth(l) max depth(r)) + 1
    }
    
    def map[T, U](t: MyTree[T])(f: T => U): MyTree[U]= t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
    
    def fold[T, A](t: MyTree[T], z: A)(acc: (A, T) => A): A = t match { 
        case Leaf(v) => acc(z, v)
        case Branch(l ,r) => fold(r, fold(l, z)(acc))(acc)
    } 
    
    def fold[A,B](t: MyTree[A])(f: A => B)(g: (B,B) => B): B = t match {
        case Leaf(v) => f(v)
        case Branch(l ,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
}