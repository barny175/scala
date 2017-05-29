package fpinscala.errorhandling

import fpinscala._
import fpinscala.datastructures._
import fpinscala.Animal

object Ex4 extends App {
    val o: MyOption[Int] = Some(12)
    println(o)
    println(o.map { x => x + 100 })
    
    println(o.flatMap { x => Some(x * 100) })
    println(o.flatMap { _ => None })
    
    val pet: MyOption[Animal] = Some(new Dog())
    println(pet)
    
    val noPet: MyOption[Animal] = None
    println(pet.getOrElse(new Cat()))
    val np: Animal = noPet.getOrElse(new Cat())
    println(np)
    
    println(pet.orElse(new Cat()))
    println(noPet.orElse(new Cat()))
    
    println(o.filter { x => x == 12 })
    println(o.filter { x => x > 12 })
    
//    def variance(xs: Seq[Double]): MyEither[String, Double] = {
//        mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
//    }
    
    println("-----------------")
    println(MyOption.sequence(List(Some(12), Some(5))))
    println(MyOption.sequence(List(Some(12), None, Some(5))))
    println(MyOption.sequence2(List(Some(12), None, Some(5))))
    println(MyOption.traverse(List(12, 5, 6, 9, 11, -4))(x => if (x % 2 == 0) None else Some(x)))
    println(MyOption.traverse(List(12, 5, 6, 9, 11, -4))(x => Some(x / 2)))
    
    println("-----------------")
    val left:MyEither[String, Int] = Left("Left is always wrong.")
    println(left)
    val right: MyEither[String, Int] = Right(12)
    println(right)
    println(right map {_ * 3})
    
    val divBy3 = (i:Int) => {
        if (i == 0) Left("Division by zero")
        else Right(i / 3)
    }
    println(left flatMap divBy3)
    println(Right(0) flatMap divBy3)
    println(right flatMap divBy3)
    
    def mean(xs: Seq[Double]): MyEither[String, Double] = {
        if (xs.isEmpty) Left("Empty list")
        else Right(xs.sum/xs.length)
    }
    
    println(mean(scala.collection.Seq()))
    println(mean(scala.collection.Seq(12.0, 4.0, 8)))
}