pragma solidity >=0.4.10 <0.6.0;

library Search {
    function indexOf(uint[] storage self, uint value)
        public
        
        returns (uint)
    {
        for (uint i = 0; i < self.length; i++)
            if (self[i] == value) return i;
        return uint(-1);
    }
    
    function incr(uint x) returns (uint) {
        return x + 1;
    }
}

contract C {
    using Search for *;
    uint[] data;

    function append(uint value) public {
        data.push(value);
    }

    function replace(uint _old, uint _new) public {
        // This performs the library function call
        uint index = data.indexOf(_old);
        if (index == uint(-1))
            data.push(_new);
        else
            data[index] = _new;
    }
}