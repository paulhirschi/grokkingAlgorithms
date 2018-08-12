/** 
 * def binarySearch: List[Int] => Int => Option[(Int, Int)]
 * -- Tail recursive function to perform a binary search on a List[Int]

 * def maxNumberSteps: Int => (Double, Int)
 * -- function evaluates to the maximum number of steps it would take to find the
 * -- index of an item in a List of length (n: Int)

 * TODO: Make the function polymorphic
 */

val theList: List[Int] = (1 to 100).toList

// Evaluates to (indexOfTarget: Int, numberStepsToFind: Int)
def binarySearch(list: List[Int])(target: Int): Option[(Int, Int)] = {

  // binary search only works on a sorted sequence
  // I know using a Range to create the List will already be sorted
  val sortedList = list.sorted

  @annotation.tailrec
  def go(low: Int, high: Int, steps: Int): Option[(Int, Int)] = {
    (low + high) / 2 match {
      case _ if high < low => None
      case mid if sortedList(mid) == target => Some(mid, steps)
      case mid if sortedList(mid) > target => go(low, mid - 1, steps + 1)
      case mid if sortedList(mid) < target => go(mid + 1, high, steps + 1)
    }
  }

  go(0, sortedList.indices.last, 1)
}

// Binary Search is O(log n)
def maxNumberSteps(n: Int): (Double, Int) = {
  import scala.math.{log10, ceil}
  val log2 = (x: Double) => log10(x)/log10(2.0)
  (log2(n), ceil(log2(n)).toInt)
}

val find = binarySearch(theList)(_)

println(find(50)) // should take 1 step to find
println(find(25)) // should take 2 steps to find
println(find(75)) // should take 2 steps to find
println(find(12)) // should take 3 steps to find
println(find(88)) // should take 3 steps to find
println(find(101)) // should be None
println(maxNumberSteps(theList.length)) // should be 7 max steps to find index of value
