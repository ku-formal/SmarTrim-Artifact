from z3 import *

def test1(i):
    a, b = BitVecs('a b', i)
    s = Solver()
    ret = s.check(UGT(a, 1), UGT(b, a), a * b == -1, UDiv(-1, a) == b)
    if ret:
        m = s.model()
    else:
        m = None
    print(i, ret, m)
    
    
def test2(i):
    a, b, c = BitVecs('a b c', i)
    s = Solver()
    ret = s.check(UGT(a, 1), UGT(b, a), UGT(c, b), a * b * c == -1, UDiv(UDiv(-1, a), b) == c)
    if ret:
        m = s.model()
    else:
        m = None
    print(i, ret, m)
    
    
def quantif():
    x, y, y1, y2 = BitVecs('x y y1 y2', 64)
    s = Solver()
    s.add(x == 0, y == y1, y1 <= 10, ForAll([y2], Not (And (x == 0, y == y2, y2 <= 20))))
    print(s.check())
    

def main():
    for i in range(1, 40):
        test2(i)


if __name__ == '__main__':
    quantif()