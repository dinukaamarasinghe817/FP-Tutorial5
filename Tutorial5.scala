object tutorial5{
    def main(args: Array[String])={
        args(0) match{
            case "q1" => {
                            //method of greatest common devisor
                                def GCD(a:Int,b:Int):Int = b match{
                                    case 0 => a
                                    case x if x>a => GCD(b,a)
                                    case x => GCD(x,a%x)
                                }
                            //method to check whether it's a prime number
                                def isPrime(x:Int,n:Int=2):Boolean = n match{
                                    case n if(n==x) => true
                                    case n if(GCD(x,n)>1) => false
                                    case n => isPrime(x,n+1)
                                }
                                print("Enter a number : ")
                                var input = scala.io.StdIn.readInt()
                                isPrime(input) match{
                                    case true => print("Entered number is a prime number")
                                    case false => print("Entered number is not a prime number")
                                }
                        }

            case "q2" => {  
                            //method of greatest common devisor
                                def GCD(a:Int,b:Int):Int = b match{
                                    case 0 => a
                                    case x if x>a => GCD(b,a)
                                    case x => GCD(x,a%x)
                                }
                            //method to check whether it's a prime number
                                def isPrime(x:Int,n:Int=2):Boolean = n match{
                                    case n if(n==x) => true
                                    case n if(GCD(x,n)>1) => false
                                    case n => isPrime(x,n+1)
                                }
                            //method to print all the prime numbers up to a certain number
                                def printPrime(x:Int):Any = x match{
                                    case x if x>1 => {printPrime(x-1); 
                                                    if(isPrime(x)){print(x.toString+" ");}}
                                    case 1 => print("")
                                }
                                print("Enter a number : ")
                                var input = scala.io.StdIn.readInt()
                                print("Prime numbers up to "+input.toString()+" are : ")
                                printPrime(10)
                        }

            case "q3" => {
                            // method to print the summation of all the numbers up to a given number
                                def sum(x:Int):Int = x match {
                                    case x if x>1 => x+sum(x-1)
                                    case x => x
                                }
                                print("Enter a number : ")
                                var input = scala.io.StdIn.readInt()
                                print("The sum of first "+input.toString()+" numbers is : ")
                                print(sum(5))
                        }

            case "q4" => {
                            // method to identify if it's an even number
                                def iseven(x:Int):Boolean = x match {
                                    case 1 => false
                                    case x => isodd(x-1)
                                }
                            // method to identify if it's an odd number
                                def isodd(x:Int):Boolean = x match {
                                    case 1 => true
                                    case x => iseven(x-1)
                                }
                                print("Enter a number : ")
                                var input = scala.io.StdIn.readInt()
                                iseven(input) match{
                                    case true => print(input.toString()+" is an even number")
                                    case false => print(input.toString()+" is an odd number")
                                }
                        }

            case "q5" => {
                            // method to identify if it's an even number
                                def iseven(x:Int):Boolean = x match {
                                    case 1 => false
                                    case x => isodd(x-1)
                                }
                            // method to identify if it's an odd number
                                def isodd(x:Int):Boolean = x match {
                                    case 1 => true
                                    case x => iseven(x-1)
                                }
                            // method to add all the even numbers below a particular given number
                                def addeven(x:Int):Int = x match{
                                    case 0 => 0
                                    case x if iseven(x) => x+addeven(x-1)
                                    case x => addeven(x-1)
                                }
                                print("Enter a number : ")
                                var input = scala.io.StdIn.readInt()
                                print("Sum of all even numbers below "+input.toString()+" is : ")
                                print(addeven(input-1))
                        }

            case "q6" => {
                            // method to calculate nth fibonacci number
                                def fibonacci(x:Int):Int = x match{
                                    case 1 => 0
                                    case 2 => 1
                                    case _ => fibonacci(x-1)+fibonacci(x-2)
                                }
                            // method to print first n fibonacci numbers 
                                def printfibonacci(x:Int):Unit = x match{
                                    case 1 => print(fibonacci(1).toString+" ")
                                    case x => printfibonacci(x-1); print(fibonacci(x).toString+" ");
                                }
                                print("Enter a number : ")
                                var input = scala.io.StdIn.readInt()
                                print("First "+input.toString()+" fibonacci numbers are : ")
                                printfibonacci(10)
                        }
        }
    }
}