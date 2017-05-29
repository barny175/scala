trait Greet {
    def greet(): String
}

trait HelloGreet extends Greet {
    def greet(): String = {
        return "Hello"
    }
}

class GreetService extends Greet with HelloGreet {
    
}

object Mixin {
    def main(args: Array[String]) {
        println(new GreetService().greet())
    }
}