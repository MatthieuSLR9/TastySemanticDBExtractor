class TastyQueryLists {

  val syntheticList = List(2, 4, 5)
  val syntheticList2 = syntheticList ++ syntheticList
  val syntheticList3 = List("foo", "foo2") ++ List(6, 7, 8)
  val syntheticList4 = List(List(1, 2, 3), List(200, 400, 500))

  def flatMap(): List[String] = {
    syntheticList4.flatMap(innerList =>
      innerList.map(x => if (x > 0) 1 else 0)
    )
    syntheticList4.flatMap(innerList =>
      innerList.map(x => if (x %2 == 1) "1" else "0").filter(x => x.length != 1)
    )
  }

  def mapDouble(): List[Int] = {
    syntheticList.map(x => x * 2)
  }

  def firstElement(): Any = {
    syntheticList.headOption.getOrElse("List is empty")
  }

  def recursion(x: List[Int]): String = {
    x match {
      case _ :: tail => recursion(tail)
      case Nil => "1"
    }
  }

  def recursionResult(): String = {
    recursion(syntheticList)
  }
}
