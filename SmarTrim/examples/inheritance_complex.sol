contract D {
    event print(string);
    function test () public {
        emit print("D");
    }
}

contract C {
    event print(string);
    function test () public {
        emit print("C");
    }
}

contract B is D {
    function test () public {
        super.test();
    }
}

contract A is D, C, B {
    function test () public {
        super.test(); // when deploying A, should print 'C' 
    }
}
