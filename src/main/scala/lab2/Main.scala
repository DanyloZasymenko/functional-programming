package lab2

object Main {

  type Set = Int => Boolean
  val SetDisplayBound = 20
  val SetForAllBound = 1000
  val NegativeSet: Set = (x: Int) => x < 0
  val PositiveSet: Set = (x: Int) => x > 0
  val ZeroSet: Set = singletonSet(0)

  def singletonSet(elem: Int): Set = (x: Int) => x == elem

  def main(args: Array[String]): Unit = {
    println("Negative Set: ")
    printSet(NegativeSet)
    println("Positive Set: ")
    printSet(PositiveSet)
    println("Zero Set: ")
    printSet(ZeroSet)

    println()

    val Set1: Set = createSet(List(3, 5, 7, -2))
    val Set2: Set = createSet(List(-10, 5, -2))
    println("Set1:")
    printSet(Set1)
    println("Set2:")
    printSet(Set2)
    println("Union:")
    printSet(union(Set1, Set2))
    println("Intersection:")
    printSet(intersect(Set1, Set2))
    println("Difference:")
    printSet(diff(Set1, Set2))
    println("Filter: s1 > 4")
    printSet(filter(Set1, (x: Int) => x > 4))
    println("Filter: s2 % 5 == 0")
    printSet(filter(Set2, (x: Int) => x % 5 == 0))

    println()

    val TenSet: Set = (x: Int) => x % 10 == 0
    println("TenSet: ")
    printSet(TenSet)
    println("forall x % 2 == 0: " + forall(TenSet, x => x % 2 == 0))
    println("forall x % 3 == 0: " + forall(TenSet, x => x % 3 == 0))
    println("exists x % 3 == 0: " + exists(TenSet, x => x % 3 == 0))
    println("exists x % 5 == 1: " + exists(TenSet, x => x % 5 == 1))

    println()

    val TenMinusTwoSet: Set = map(TenSet, x => x - 2)
    println("TenMinusTwoSet: ")
    printSet(TenMinusTwoSet)
  }

  def createSet(ints: List[Int]): Set = (x: Int) => ints.contains(x)

  def printSet(s: Set): Unit = {
    println((-SetDisplayBound to SetDisplayBound).filter(x => contains(s, x)).mkString("Set[", ", ", "]"))
  }

  def union(s1: Set, s2: Set): Set = (x: Int) => contains(s1, x) || contains(s2, x)

  def intersect(s1: Set, s2: Set): Set = (x: Int) => contains(s1, x) && contains(s2, x)

  def diff(s1: Set, s2: Set): Set = (x: Int) =>
    (contains(s1, x) && !contains(s2, x)) || (!contains(s1, x) && contains(s2, x))

  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => s(x) && p(x)

  def map(s: Set, f: Int => Int): Set = x => exists(s, y => x == f(y))

  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def foreach(elem: Int): Boolean = {
      if (elem > SetForAllBound) {
        true
      } else if (contains(s, elem) && !p(elem)) {
        false
      } else {
        foreach(elem + 1)
      }
    }

    foreach(-SetForAllBound)
  }

  def contains(s: Set, elem: Int): Boolean = s(elem)

}
