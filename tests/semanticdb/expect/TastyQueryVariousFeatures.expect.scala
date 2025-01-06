class VariousFeatures/*<-_empty_::VariousFeatures#*/(param1/*<-_empty_::VariousFeatures#param1.*/: Double/*->scala::Double#*/ |/*->scala::`|`#*/ String/*->scala::Predef.String#*/, param2/*<-_empty_::VariousFeatures#param2.*/: Int/*->scala::Int#*/){
    val x/*<-_empty_::VariousFeatures#x.*/ : String/*->scala::Predef.String#*/ &/*->scala::`&`#*/ Int/*->scala::Int#*/ = ???/*->scala::Predef.`???`().*/
    def usingClassParams/*<-_empty_::VariousFeatures#usingClassParams().*/ = param1/*->_empty_::VariousFeatures#param1.*/ match
        case param1/*<-local0*/: Double/*->scala::Double#*/ =>
            param1/*->local0*/.toInt/*->scala::Double#toInt().*/ +/*->scala::Int#`+`(+4).*/ param2/*->_empty_::VariousFeatures#param2.*/
        case _ =>
            val /*** Some comments*/ someLocalVal/*<-local1*/ = 3
            val otherLocalVal/*<-local2*/ = 4
            if param2/*->_empty_::VariousFeatures#param2.*/ >/*->scala::Int#`>`(+3).*/ 10 then
                (someLocalVal/*->local1*/ */*->scala::Int#`*`(+3).*/ otherLocalVal/*->local2*/).toDouble/*->scala::Int#toDouble().*/
            else
                None/*->scala::None.*/
    def modfiyString/*<-_empty_::VariousFeatures#modfiyString().*/(inputString/*<-_empty_::VariousFeatures#modfiyString().(inputString)*/: String/*->scala::Predef.String#*/): String/*->scala::Predef.String#*/ =
        param1/*->_empty_::VariousFeatures#param1.*/ match
            case string/*<-local3*/: String/*->scala::Predef.String#*/ => string/*->local3*/.concat/*->java::lang::String#concat().*/(inputString/*->_empty_::VariousFeatures#modfiyString().(inputString)*/)
            case double/*<-local4*/: Double/*->scala::Double#*/ => inputString/*->_empty_::VariousFeatures#modfiyString().(inputString)*/.charAt/*->java::lang::String#charAt().*/(double/*->local4*/.toInt/*->scala::Double#toInt().*/).toString/*->scala::Any#toString().*/

    val modified/*<-_empty_::VariousFeatures#modified.*/ = modfiyString/*->_empty_::VariousFeatures#modfiyString().*/("Hello")

    def incompleteFormula/*<-_empty_::VariousFeatures#incompleteFormula().*/(param1/*<-_empty_::VariousFeatures#incompleteFormula().(param1)*/: Int/*->scala::Int#*/, param2/*<-_empty_::VariousFeatures#incompleteFormula().(param2)*/: Double/*->scala::Double#*/, param3/*<-_empty_::VariousFeatures#incompleteFormula().(param3)*/: Float/*->scala::Float#*/) =
        if {param3/*->_empty_::VariousFeatures#incompleteFormula().(param3)*/ %/*->scala::Float#`%`(+5).*/ (param1/*->_empty_::VariousFeatures#incompleteFormula().(param1)*/ +/*->scala::Int#`+`(+6).*/ param3/*->_empty_::VariousFeatures#incompleteFormula().(param3)*/)//*->scala::Float#`::`(+6).*/param2/*->_empty_::VariousFeatures#incompleteFormula().(param2)*/}.toInt/*->scala::Double#toInt().*/ ==/*->scala::Int#`==`(+3).*/ 3 then
            ???/*->scala::Predef.`???`().*/
        else
            param1/*->_empty_::VariousFeatures#incompleteFormula().(param1)*/ ^/*->scala::Int#`^`(+3).*/ param1/*->_empty_::VariousFeatures#incompleteFormula().(param1)*/
}

//another comment
class VariousFeatures2/*<-_empty_::VariousFeatures2#*/{
    val y/*<-_empty_::VariousFeatures2#y.*/ = "Hello World"
    def printHelloWorld/*<-_empty_::VariousFeatures2#printHelloWorld().*/ = print/*->scala::Predef.print().*/(y/*->_empty_::VariousFeatures2#y.*/)
}
