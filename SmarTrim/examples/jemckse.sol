contract TestMe {
    uint x; bool flag;
    bool trapSwitch; uint trapCounter;
    modifier trapSwitchIsOff {
        require(!trapSwitch) ;_; // sad face
    }
    function X(uint y) trapSwitchIsOff() external {x=y;}
    function F() trapSwitchIsOff() external {require(!flag); flag=true;}
    function test() trapSwitchIsOff() external {require(x == 10 && flag == true); assert(false);}
    function trap() trapSwitchIsOff() external {trapSwitch = true;}
    function kidnap() external {require(trapSwitch); trapCounter++;}
}