class TastyQueryVar/*<-_empty_::TastyQueryVar#*/{
    var x/*<-_empty_::TastyQueryVar#x().*/ = 2
    x/*->_empty_::TastyQueryVar#`x_=`().*/ = 3

    def foo/*<-_empty_::TastyQueryVar#foo().*/ =
        var x/*<-local0*/ = 2
        x/*->local0*/ = 3
    var p/*<-_empty_::TastyQueryVar#p().*/ = foo/*->_empty_::TastyQueryVar#foo().*/
    p/*->_empty_::TastyQueryVar#`p_=`().*/ = {val x/*<-local1*/ = true}
}
