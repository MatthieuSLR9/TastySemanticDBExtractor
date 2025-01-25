class TastyQueryVar/*<-_empty_::TastyQueryVar#*/{
    var x/*<-_empty_::TastyQueryVar#x().*/ = 2
    x/*->_empty_::TastyQueryVar#`x_=`().*/ = 3

    def foo/*<-_empty_::TastyQueryVar#foo().*/ =
        var z/*<-local0*/ = 2
        z/*->local0*/ = 3
    var p/*<-_empty_::TastyQueryVar#p().*/ = foo/*->_empty_::TastyQueryVar#foo().*/
    p/*->_empty_::TastyQueryVar#`p_=`().*/ = {val x/*<-local1*/ = true}
}
