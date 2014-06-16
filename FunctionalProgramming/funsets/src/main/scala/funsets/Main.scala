package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  val set_a1 = union(singletonSet(20), singletonSet(-30))
  val set_a2 = union(singletonSet(11), singletonSet(-9))
  val set_a = union(set_a1, set_a2)

  /* set_b has all elements of set_a but squared */
  val set_b1 = union(singletonSet(400), singletonSet(900))
  val set_b2 = union(singletonSet(121), singletonSet(81))
  val set_b = union(set_b1, set_b2)

  val set_c = map(set_a, x => x*x)
  print("set_a "); printSet(set_a)
  print("set_b "); printSet(set_b)
  print("set_c "); printSet(set_c)

  println("set_b same as set_c = " + same(set_b, set_c))
}
