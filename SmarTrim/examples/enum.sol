contract A {

    enum Action {A1, A2, A3}
}


contract B is A {
  enum Action2 {A11, A22, A33}
    event log (uint n);
    
    function test () public {
	  Action aaa = Action.A3;
	  require (Action2.A33 >= Action2.A22);
	  require (Action.A1 >= Action.A2);
	  require (aaa >= Action.A1);
	  assert (1+1>=1);
    }
}


contract Test{
    enum MyEnum {AA, BB, CC}
    enum MyEnum2 {AA, BB, CC}
    
    MyEnum my;
    function test() public {
        assert (MyEnum.AA == MyEnum(0)); // proven
    }
    
    event show (MyEnum);
    
    function test2 () public {
        emit show (my); // log 0
        emit show (MyEnum(1)); // log 1
        assert (MyEnum.BB == MyEnum(1)); // proven
    }
    
    function test3 () public {
        assert (uint(MyEnum2.AA) == uint(MyEnum.AA)); // proven
    }
    
    function test4() public {
        assert (uint(MyEnum2.AA) == uint(MyEnum.BB)); // unproven
    }

    function test5() public returns(MyEnum ret) {
        assert (ret == MyEnum.AA);
    }
}
