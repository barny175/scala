case class ListNode[+T](h: T, t: ListNode[T]) {
    def head: T = h
    def tail: ListNode[T] = t
    def prepend[U >: T](elem: U): ListNode[U] =
        ListNode(elem, this)
}

object ListNode extends App {
    var list: ListNode[String] = ListNode("hello", null)
    var ol:ListNode[Object] = list
    var ol2 = ol.prepend(123)
    println(ol)
    println(ol2)
    println(list)
}