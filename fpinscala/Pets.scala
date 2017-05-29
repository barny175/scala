package fpinscala

abstract class Animal {}

abstract class Pet extends Animal {}

class Dog extends Pet {
    override def toString():String = "Dog"
}

class Cat extends Pet {
    override def toString():String = "Cat"
}

class Cage[P <: Pet](p: P){
}

class Lion extends Animal {
}

object Pets extends App {
    var dc = new Cage(new Dog)
    var cc = new Cage(new Cat)
//    var lc = new Cage(new Lion) 
}