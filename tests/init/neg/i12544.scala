enum Enum:
  case Case
  case Case2(x: Int)

class Outer:
  val e = new Enum2 // error
  def g(b: e.B): Int = b.foo()

  class Enum2:
    class nested:
      val a: Enum = Enum.Case

    val b: Enum = f((new nested).a)

    def f(e: Enum): Enum = e

    class B() { def foo() = n + 1 }
    g(new e.B())
    val n: Int = 10

@main def main(): Unit = {
  val o = new Outer
  print(o.e.b)
}
