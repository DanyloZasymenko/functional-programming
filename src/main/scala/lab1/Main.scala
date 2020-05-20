package lab1

object Main {

  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || r == 1 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  def balance(input: String): Boolean = {
    def balance(input: List[Char], count: Int): Boolean = {
      if (count < 0) {
        false
      } else if (input.isEmpty) {
        count == 0
      } else if (input.head == '(') {
        balance(input.tail, count + 1)
      } else if (input.head == ')') {
        balance(input.tail, count - 1)
      } else {
        balance(input.tail, count)
      }
    }

    balance(input.toList, 0)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0 || coins.isEmpty || (money <= 0 && coins.nonEmpty)) {
      0
    } else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }

  def main(args: Array[String]): Unit = {
    println(pascal(0, 2))
    println(pascal(1, 2))
    println(pascal(1, 3))

    println()

    println(balance("(a (0? b) max (/ 2 z))"))
    println(balance("Bla bla (bla (bla) bla)"))
    println(balance(":-)"))
    println(balance("())("))

    println()

    println(countChange(4, List(1, 2)))
  }

}
