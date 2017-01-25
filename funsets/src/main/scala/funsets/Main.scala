package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(1), 2))

  printSet(union(singletonSet(1), singletonSet(2)))
  printSet(map(union(singletonSet(1), singletonSet(2)), v => v * 2))
  println(exists(union(singletonSet(1), singletonSet(2)), v => v == 2))
  println(exists(union(singletonSet(1), singletonSet(2)), v => v == 1))
  println(exists(union(singletonSet(1), singletonSet(2)), v => v == 0))
  println(forall(union(singletonSet(1), singletonSet(2)), v => v > 0))
  println(forall(union(singletonSet(1), singletonSet(2)), v => v == 1))
  println(forall(union(singletonSet(1), singletonSet(2)), v => v == 2))
  println(forall(union(singletonSet(1), singletonSet(2)), v => v == 2 || v == 1))
}
