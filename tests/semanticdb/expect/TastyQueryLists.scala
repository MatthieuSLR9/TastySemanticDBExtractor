class TastyQueryListSynthetics {
  val syntheticList = List(2, 4, 5)
  val syntheticList2 = syntheticList ++ syntheticList

  val first = syntheticList(0)
  def recursion(x :List[Int]) : String =
    x match
        case head :: next => recursion(next)
        case Nil => "1"
    
  syntheticList match
    case head :: next => print("1")
    case Nil => false

}
