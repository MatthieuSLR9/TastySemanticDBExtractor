class MethodWithParamMultiply{
    def addition(z: Int, y: Int) = z+y
    def multiplication(x:Int, y: Int) = x*y
    def division(x:Int, y:Int) = x/y
    def power(x:Int, y:Int) = x^y
    def complexFormula(param1: Int, param2: Double, param3: Float) =
        val intermidiate = {param3 * (param1 + param3)/param2}.toInt
        intermidiate ^ intermidiate
}