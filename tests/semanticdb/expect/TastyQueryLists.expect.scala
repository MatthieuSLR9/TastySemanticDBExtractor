class TastyQueryLists/*<-_empty_::TastyQueryLists#*/ {

  val syntheticList/*<-_empty_::TastyQueryLists#syntheticList.*/ = List/*->scala::package.List.*/(2, 4, 5)
  val syntheticList2/*<-_empty_::TastyQueryLists#syntheticList2.*/ = syntheticList/*->_empty_::TastyQueryLists#syntheticList.*/ ++/*->scala::collection::IterableOps#`++`().*/ syntheticList/*->_empty_::TastyQueryLists#syntheticList.*/
  val syntheticList3/*<-_empty_::TastyQueryLists#syntheticList3.*/ = List/*->scala::package.List.*/("foo", "foo2") ++/*->scala::collection::IterableOps#`++`().*/ List/*->scala::package.List.*/(6, 7, 8)
  val syntheticList4/*<-_empty_::TastyQueryLists#syntheticList4.*/ = List/*->scala::package.List.*/(List/*->scala::package.List.*/(1, 2, 3), List/*->scala::package.List.*/(200, 400, 500))

  def flatMap/*<-_empty_::TastyQueryLists#flatMap().*/(): List/*->scala::package.List#*/[String/*->scala::Predef.String#*/] = {
    syntheticList4/*->_empty_::TastyQueryLists#syntheticList4.*/.flatMap/*->scala::collection::immutable::List#flatMap().*/(innerList/*<-local0*/ =>
      innerList/*->local0*/.map/*->scala::collection::immutable::List#map().*/(x/*<-local1*/ => if (x/*->local1*/ >/*->scala::Int#`>`(+3).*/ 0) 1 else 0)
    )
    syntheticList4/*->_empty_::TastyQueryLists#syntheticList4.*/.flatMap/*->scala::collection::immutable::List#flatMap().*/(innerList/*<-local2*/ =>
      innerList/*->local2*/.map/*->scala::collection::immutable::List#map().*/(x/*<-local3*/ => if (x/*->local3*/ %/*->scala::Int#`%`(+3).*/2 ==/*->scala::Int#`==`(+3).*/ 1) "1" else "0").filter/*->scala::collection::immutable::List#filter().*/(x/*<-local4*/ => x/*->local4*/.length/*->java::lang::String#length().*/ !=/*->scala::Int#`!=`(+3).*/ 1)
    )
  }

  def mapDouble/*<-_empty_::TastyQueryLists#mapDouble().*/(): List/*->scala::package.List#*/[Int/*->scala::Int#*/] = {
    syntheticList/*->_empty_::TastyQueryLists#syntheticList.*/.map/*->scala::collection::immutable::List#map().*/(x/*<-local5*/ => x/*->local5*/ */*->scala::Int#`*`(+3).*/ 2)
  }

  def firstElement/*<-_empty_::TastyQueryLists#firstElement().*/(): Any/*->scala::Any#*/ = {
    syntheticList/*->_empty_::TastyQueryLists#syntheticList.*/.headOption/*->scala::collection::LinearSeqOps#headOption().*/.getOrElse/*->scala::Option#getOrElse().*/("List is empty")
  }

  def recursion/*<-_empty_::TastyQueryLists#recursion().*/(x/*<-_empty_::TastyQueryLists#recursion().(x)*/: List/*->scala::package.List#*/[Int/*->scala::Int#*/]): String/*->scala::Predef.String#*/ = {
    x/*->_empty_::TastyQueryLists#recursion().(x)*/ match {
      case _ ::/*->scala::package.`::`.*/ tail/*<-local6*/ => recursion/*->_empty_::TastyQueryLists#recursion().*/(tail/*->local6*/)
      case Nil/*->scala::package.Nil.*/ => "1"
    }
  }

  def recursionResult/*<-_empty_::TastyQueryLists#recursionResult().*/(): String/*->scala::Predef.String#*/ = {
    recursion/*->_empty_::TastyQueryLists#recursion().*/(syntheticList/*->_empty_::TastyQueryLists#syntheticList.*/)
  }
}
