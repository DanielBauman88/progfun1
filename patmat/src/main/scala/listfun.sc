def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (first, rest) = xs.span(elem => elem == x)
    first :: pack(rest)
  }
}

def runLength[T](xs: List[T]): List[(T, Int)] = {
  pack(xs).map(x => (x.head, x.size))
}

val nums = List(2, -4, 5, -7, 1)
val fruits = List("apple","pineapple","orange", "banana")
val testPack = List("a", "a", "b", "c", "c", "a")

//nums.filter(x => x > 0)
nums.filterNot(x => x > 0)
nums.partition(x => x > 0)


nums.takeWhile(x => x > 0)
nums.dropWhile(x => x > 0)


nums.span(x => x > 0)


pack(testPack)
runLength(testPack)

List(2, 2).reduceLeft(_ * _)
List(2, 2).reduceRight(_ * _)

def doReduce(arg: List[Int]): Int = {
  arg.foldLeft(-50)(_ + _)
}

doReduce(List())
doReduce(List(1, 2))

5 :: List(5)

List(1, 2) zip Nil :: Nil
List('a') zip List(1, 2)

List(1,2).flatMap(x => x*2)
