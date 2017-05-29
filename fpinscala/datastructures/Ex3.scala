package fpinscala.datastructures

object Ex3 extends App {
  val x = List(1, 2, 3, 4, 5) match {
        case Cons(x, Cons(2, Cons(4, _)))          => x
        case Nil                                   => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t)                            => h + List.sum(t)
        case _                                     => 101
    }    
    println(x)
    
    val l = List(1, 2, 3, 4, 5)
    println(List.tail(l))
    
    println(List.setHead(12, l))
    
    println(List.drop(l, 2))
    
    println(List.dropWhile(l, (x:Int) => x < 3))
    println(List.dropWhile(l, (x:Int) => x  > 3))
    println(List.takeWhile(l)((x:Int) => x  <= 3))
    
    println(List.init(l))
    
    println(List.length(l))
    println(List.length2(l))
    
    println(List.foldLeft(l, 0)(_+_))
    
    println(List.foldLeft(l, 0)(_-_))
    println(List.foldLeft2(l, 0)(_-_))
    println(List.foldRight(l, 0)(_-_))
    println(List.foldRight2(l, 0)(_-_))
    
    println(List.reverse(l))
    
    val l2 = List(-5,-12,34);
    println(List.concat(List(l, l2)));
    
    println(List.map(l)(_+1))
    println(List.map2(l)(_+1))
    
    println(List.filter(l)(_ % 2 == 0))
    println(List.filter2(l)(_ % 2 == 0))
    
    println(List.flatMap(l)(i => List(-i, i)))
    
    println(List.zipWith(l, l2)(_-_))
    
//    println(List.take(l, 3) == List(1, 2, 3))
    
    println(List.hasSubsequence(l, List(3, 4)))
    println(List.hasSubsequence(l, List(3, 4, 5)))
    println(List.hasSubsequence(l, List(3, 5)))
    println(List.hasSubsequence(l, l))
    
    println("-------------------");
    
    val myTree = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(13))), Leaf(12))
    println(myTree)
    println(MyTree.size(myTree))
    println(MyTree.maximum(myTree))
    
    println(MyTree.depth(myTree))
    
    println(MyTree.map(myTree)(_+1))
    
    println(MyTree.fold(myTree, 0)(_+_))
    println(MyTree.fold(myTree, 0)((a,_) => a + 1))
    println(MyTree.fold(myTree, 0)(_ max _))
    
    println("-------------------");
    println(MyTree.fold(myTree)(v => v)(_+_))
    println(MyTree.fold(myTree)(v => 1)((a,b) => a + b))
    println(MyTree.fold(myTree)(v => v)((a,b) => a max b))
    
    // depth
    println(MyTree.fold(myTree)(v => 0)((a,b) => (a max b) + 1))
    
    // size
    println(MyTree.fold(myTree)(v => 1)((a,b) => a + b + 1))
}