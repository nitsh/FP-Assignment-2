package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(r: Int, n: Int): Int = {
    if(r == 0) 1
    else n*(pascal(r-1, n-1))/r
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars:List[Char], openBrackets:Int):Boolean = chars match {
      case Nil => {
        if(openBrackets == 0){
          true
        }else{
          false
        }
      }
      case head :: tail =>{
        if(openBrackets < 0){
          false
        }else if(head == '('){
          balance(tail, openBrackets+1)
        }else if(head == ')'){
          balance(tail, openBrackets-1)
        }else {
          balance(tail, openBrackets)
        }
      }
    }
    balance(chars , 0)
  }

  def breakMoney(money: Int, coins: List[Int], acc:Int): Int = coins match {
    case Nil => acc
    case head::tail => breakMoney(money, tail, acc + countChange(money-head, coins))
  }

  /**
   *
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money < 0)0
    else if(money == 0) 1
    else {
      breakMoney(money, coins, 0)
    }
  }
}
