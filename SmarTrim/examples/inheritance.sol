contract D{
	uint d;
	function D () public{
		d =3;

	}

    function testa() public{
        
    }
}

contract C {
    event cc (uint n);
    function test () public{
        emit cc(3);
    }
    
}

contract B is D {
    event bb(uint n);
    //function test() public{
    //    emit bb (2);
    //}
}

contract A is C,B{
    event aa (uint n);
    
    function test () public{
        emit aa(1);
        super.test();

        /* not possible */
        // B.testa();

        /* possible */
        // B b = new B();
        // b.testa();
    }
}
