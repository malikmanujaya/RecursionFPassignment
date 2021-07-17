object recursionFP extends App{
  def gcd(x : Int,y :Int):Int = y match {
    case 0 => x
    case y if (y>x) => gcd(y,x)
    case _ => gcd(y,x%y)
  }
  def isprime(p:Int,n:Int=2):Boolean = n match{
    case x if(p==x) =>true
    case x if (gcd(p,x)>1) => false
    case x => isprime(p,x+1)
  }
  def primeSeq(n:Int):Any={
    if(isprime(n)==true ) print(n+"\n")
    if (n>0) primeSeq(n-1)
  }
  def issum(n:Int):Int={
    if(n==1) 1
    else n+issum(n-1)

  }
  def isEven(n:Int):Boolean = n match{
    case 0 => true
    case _ => isOdd(n-1)
  }

  def isOdd(n:Int):Boolean = !isEven(n)

  def Sumofn(n:Int,m:Int):Int={
    if(n>m)  0 else  n+Sumofn(n+2,m)

  }
  def fibonacci(n:Int) : Int = n match {
    case n if n==0 =>0
    case n if n==1 => 1
    case n => fibonacci(n-1)+fibonacci(n-2)
  }
  def fibonacciseq(n:Int):Any = {
    if(n>0) fibonacciseq(n-1)
    println(fibonacci(n))
  }

  print("Question 01 : \n")
  print(isprime(3)+"\n")
  print(isprime(8)+"\n")
  print("\n")
  print("Question 06 : \n")
  fibonacciseq(10)
  print("\n")
  print("Question 05 : \n")
  print(Sumofn(0,10))
  print("\n")
  print("Question 04 : \n")
  print(isOdd(8)+"\n")
  print(isEven(3)+"\n")
  print("\n")
  print("Question 03 : \n")
  print(issum(5)+"\n")
  print("\n")
  print("Question 02 : \n")
  primeSeq(10)

}