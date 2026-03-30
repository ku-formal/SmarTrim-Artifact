contract C {
    struct R {
        R[] rec;
    }
    
    R r;
    constructor() {
        r = R({ rec : new R[](0) });
    }
}