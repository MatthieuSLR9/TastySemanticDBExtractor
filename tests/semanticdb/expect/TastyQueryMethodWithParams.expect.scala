class MethodWithParamMultiply/*<-_empty_::MethodWithParamMultiply#*/{
    def addition/*<-_empty_::MethodWithParamMultiply#addition().*/(z/*<-_empty_::MethodWithParamMultiply#addition().(z)*/: Int/*->scala::Int#*/, y/*<-_empty_::MethodWithParamMultiply#addition().(y)*/: Int/*->scala::Int#*/) = z/*->_empty_::MethodWithParamMultiply#addition().(z)*/+/*->scala::Int#`+`(+4).*/y/*->_empty_::MethodWithParamMultiply#addition().(y)*/
    def multiplication/*<-_empty_::MethodWithParamMultiply#multiplication().*/(x/*<-_empty_::MethodWithParamMultiply#multiplication().(x)*/:Int/*->scala::Int#*/, y/*<-_empty_::MethodWithParamMultiply#multiplication().(y)*/: Int/*->scala::Int#*/) = x/*->_empty_::MethodWithParamMultiply#multiplication().(x)*/*/*->scala::Int#`*`(+3).*/y/*->_empty_::MethodWithParamMultiply#multiplication().(y)*/
    def division/*<-_empty_::MethodWithParamMultiply#division().*/(x/*<-_empty_::MethodWithParamMultiply#division().(x)*/:Int/*->scala::Int#*/, y/*<-_empty_::MethodWithParamMultiply#division().(y)*/:Int/*->scala::Int#*/) = x/*->_empty_::MethodWithParamMultiply#division().(x)*///*->scala::Int#`::`(+3).*/y/*->_empty_::MethodWithParamMultiply#division().(y)*/
    def power/*<-_empty_::MethodWithParamMultiply#power().*/(x/*<-_empty_::MethodWithParamMultiply#power().(x)*/:Int/*->scala::Int#*/, y/*<-_empty_::MethodWithParamMultiply#power().(y)*/:Int/*->scala::Int#*/) = x/*->_empty_::MethodWithParamMultiply#power().(x)*/^/*->scala::Int#`^`(+3).*/y/*->_empty_::MethodWithParamMultiply#power().(y)*/
    def complexFormula/*<-_empty_::MethodWithParamMultiply#complexFormula().*/(param1/*<-_empty_::MethodWithParamMultiply#complexFormula().(param1)*/: Int/*->scala::Int#*/, param2/*<-_empty_::MethodWithParamMultiply#complexFormula().(param2)*/: Double/*->scala::Double#*/, param3/*<-_empty_::MethodWithParamMultiply#complexFormula().(param3)*/: Float/*->scala::Float#*/) =
        val intermidiate/*<-local0*/ = {param3/*->_empty_::MethodWithParamMultiply#complexFormula().(param3)*/ */*->scala::Float#`*`(+5).*/ (param1/*->_empty_::MethodWithParamMultiply#complexFormula().(param1)*/ +/*->scala::Int#`+`(+6).*/ param3/*->_empty_::MethodWithParamMultiply#complexFormula().(param3)*/)//*->scala::Float#`::`(+6).*/param2/*->_empty_::MethodWithParamMultiply#complexFormula().(param2)*/}.toInt/*->scala::Double#toInt().*/
        intermidiate/*->local0*/ ^/*->scala::Int#`^`(+3).*/ intermidiate/*->local0*/
}