class VariousFeatures(param1: Double | String, param2: Int){
    val x : String & Int = ???
    def usingClassParams = param1 match
        case param1: Double =>
            param1.toInt + param2
        case _ =>  
            val /*** Some comments*/ someLocalVal = 3
            val otherLocalVal = 4
            if param2 > 10 then
                (someLocalVal * otherLocalVal).toDouble
            else 
                None
    def modfiyString(inputString: String): String =
        param1 match
            case string: String => string.concat(inputString)
            case double: Double => inputString.charAt(double.toInt).toString

    val modified = modfiyString("Hello")

    def incompleteFormula(param1: Int, param2: Double, param3: Float) =
        if {param3 % (param1 + param3)/param2}.toInt == 3 then 
            ???
        else 
            param1 ^ param1
}

//another comment
class VariousFeatures2{
    val y = "Hello World"
    def printHelloWorld = print(y)
}
