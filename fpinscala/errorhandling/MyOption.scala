package fpinscala.errorhandling

import fpinscala.datastructures._

sealed trait MyOption[+A] { 
    def map[B](f: A => B): MyOption[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }
    
    def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
        case None => None
        case Some(a) => f(a)
    }
    
    def flatMap2[B](f: A => MyOption[B]): MyOption[B] = 
        map(f) getOrElse None
    
    def getOrElse[B >: A](defVal: B): B = this match {
        case None => defVal
        case Some(a) => a
    }
    
    def orElse[B >:A](b: B): MyOption[B] = this match {
        case None => Some(b)
        case _ => this
    }
    
    def filter(p: A => Boolean): MyOption[A] = this match {
        case None => None
        case Some(a) => if (p(a)) this else None
    } 
}

case object None extends MyOption[Nothing] { }

case class Some[+A](get: A) extends MyOption[A] { }

object MyOption {
    def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = {
        a.flatMap{ x => b.map { y => f(x, y) } }
    }
    
    def sequence[A](as: List[MyOption[A]]): MyOption[List[A]] = as match {
        case Nil => Some(Nil)
        case Cons(h, t) => h.flatMap(hh => sequence(t).map(st => Cons(hh, st))) 
    }
    
    def sequence2[A](as: List[MyOption[A]]): MyOption[List[A]] = {
        List.foldRight[MyOption[A], MyOption[List[A]]](as, Some(Nil))((a, acc) => a.flatMap(aa => acc.map (Cons(aa, _)) ))
    }
    
    def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] = 
        List.foldRight[A, MyOption[List[B]]](as, Some(Nil))((a, acc) => f(a).flatMap(v => acc.map(Cons(v, _))))
}