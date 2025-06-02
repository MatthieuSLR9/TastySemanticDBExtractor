class TastyQueryListSynthetics/*<-_empty_::TastyQueryListSynthetics#*/ {
  val syntheticList/*<-_empty_::TastyQueryListSynthetics#syntheticList.*/ = List/*->scala::package.List.*/(2, 4, 5)
  val syntheticList2/*<-_empty_::TastyQueryListSynthetics#syntheticList2.*/ = syntheticList/*->_empty_::TastyQueryListSynthetics#syntheticList.*/ ++/*->scala::collection::IterableOps#`++`().*/ syntheticList/*->_empty_::TastyQueryListSynthetics#syntheticList.*/

  val first/*<-_empty_::TastyQueryListSynthetics#first.*/ = syntheticList/*->_empty_::TastyQueryListSynthetics#syntheticList.*/(0)
  def recursion/*<-_empty_::TastyQueryListSynthetics#recursion().*/(x/*<-_empty_::TastyQueryListSynthetics#recursion().(x)*/ :List/*->scala::package.List#*/[Int/*->scala::Int#*/]) : String/*->scala::Predef.String#*/ =
    x/*->_empty_::TastyQueryListSynthetics#recursion().(x)*/ match
        case head/*<-local0*/ ::/*->scala::package.`::`.*/ next/*<-local1*/ => recursion/*->_empty_::TastyQueryListSynthetics#recursion().*/(next/*->local1*/)
        case Nil/*->scala::package.Nil.*/ => "1"

  syntheticList/*->_empty_::TastyQueryListSynthetics#syntheticList.*/ match
    case head/*<-local2*/ ::/*->scala::package.`::`.*/ next/*<-local3*/ => print/*->scala::Predef.print().*/("1")
    case Nil/*->scala::package.Nil.*/ => false




}
