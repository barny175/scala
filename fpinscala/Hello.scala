object Hello {
    trait My {
        def isMy()
    }
    
    class MyClass extends My {
        def isMy() {
            println("It is mine.")
        }
    }
    def main(args: Array[String]) {
        println("Helo")
        new MyClass().isMy()
        
        val x:Any = "www"
        x match {
            case 1 => println("one")
            case "obj" => println(x)
            case x:String => println("object: " + x) 
        }        
    }
}