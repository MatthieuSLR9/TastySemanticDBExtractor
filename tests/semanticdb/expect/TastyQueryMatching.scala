class TastyQueryMatching{
    def matchingOr(x : Int | String | Double) = 
        x match
            case s: String => s.concat(s)
            case number: Int => (number * number)
            case double: Double => print("")
            
    def matchingDefault(x: Boolean | Unit) =
        x match
            case _: Unit => 
            case _ =>
        
}
