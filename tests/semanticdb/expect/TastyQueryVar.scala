class TastyQueryVar{
    var x = 2
    x = 3

    def foo = 
        var x = 2
        x = 3
    var p = foo
    p = {val x = true}
}
