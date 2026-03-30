/* function call example */

contract Test{
    uint x = 10;
    uint y = 10;
    uint z = 5;

    function hoo (uint h) internal returns (uint) {
        y = 2;

        y = 100+ 100;

        return 0;
    }

    function goo (uint g) internal returns (uint) {
        x = 1;

        hoo (10);
        return 0;
    }

    function foo (uint f) internal returns (uint) {
        goo (10);
        return 10;
    }

    function test(uint a, uint b){
        x = 1;
        y = 2;
        z = 3;

        foo (a);
        uint n = 1+1;

    }
}
