package example

object Main extends App {
  println("----------------")

  print("max of numbers: -2, 1, -3 = ")
  println(Lists.max(List(-2, 1, -3)))

  print("sum of numbers: -2, -1, 3, -5 = ")
  println(Lists.sum(List(-2, -1, 3, -5)))

  println("----------------")
}
