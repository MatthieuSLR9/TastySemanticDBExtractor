class TastyQueryMatching/*<-_empty_::TastyQueryMatching#*/{
    def matchingOr/*<-_empty_::TastyQueryMatching#matchingOr().*/(x/*<-_empty_::TastyQueryMatching#matchingOr().(x)*/ : Int/*->scala::Int#*/ |/*->scala::`|`#*/ String/*->scala::Predef.String#*/ |/*->scala::`|`#*/ Double/*->scala::Double#*/) =
        x/*->_empty_::TastyQueryMatching#matchingOr().(x)*/ match
            case s/*<-local0*/: String/*->scala::Predef.String#*/ => s/*->local0*/.concat/*->java::lang::String#concat().*/(s/*->local0*/)
            case number/*<-local1*/: Int/*->scala::Int#*/ => (number/*->local1*/ */*->scala::Int#`*`(+3).*/ number/*->local1*/)
            case double/*<-local2*/: Double/*->scala::Double#*/ => print/*->scala::Predef.print().*/("")

    def matchingDefault/*<-_empty_::TastyQueryMatching#matchingDefault().*/(x/*<-_empty_::TastyQueryMatching#matchingDefault().(x)*/: Boolean/*->scala::Boolean#*/ |/*->scala::`|`#*/ Unit/*->scala::Unit#*/) =
        x/*->_empty_::TastyQueryMatching#matchingDefault().(x)*/ match
            case _: Unit/*->scala::Unit#*/ =>
            case _ =>

}
