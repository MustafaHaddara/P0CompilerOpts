# coding: utf-8
# code generator for producing the AST only, with facilities for printing

from SC import mark, TIMES, DIV, MOD, AND, PLUS, MINUS, OR, EQ, NE, LT, \
     GT, LE, GE
from ST import indent, Bool

class UnaryOp:
    def __init__(self, op, arg):
        self.tp, self.op, self.arg = arg.tp, op, arg
    def __str__(self):
        return 'op' + str(self.op) + '\n' + indent(self.arg)

class BinaryOp:
    def __init__(self, op, tp, left, right):
        self.tp, self.op, self.left, self.right = tp, op, left, right
    def __str__(self):
        o = '*' if self.op == TIMES else \
            'div' if self.op == DIV else \
            'mod' if self.op == MOD else \
            'and' if self.op == AND else \
            '+' if self.op == PLUS else \
            '-' if self.op == MINUS else \
            'or' if self.op == OR else \
            '=' if self.op == EQ else \
            '<>' if self.op == NE else \
            '<' if self.op ==  LT else \
            '>' if self.op == GT else \
            '<=' if self.op == LE else \
            '>=' if self.op == GE else 'op?'
        return o + '\n' + indent(self.left) + '\n' + indent(self.right)

class Assignment:
    def __init__(self, left, right):
        self.left, self.right = left, right
    def __str__(self):
        return ':=\n' + indent(self.left) + '\n' + indent(self.right)

class Call:
    def __init__(self, ident, param):
        self.ident, self.param = ident, param
    def __str__(self):
        return 'call ' + str(self.ident) + '\n' + \
               indent('\n'.join([str(x) for x in self.param]))

class Seq:
    def __init__(self, first, second):
        self.first, self.second = first, second
    def __str__(self):
        return 'seq\n' + indent(self.first) + '\n' + indent(self.second)

class IfThen:
    def __init__(self, cond, th):
        self.cond, self.th = cond, th
    def __str__(self):
        return 'ifthen\n' + indent(self.cond) + '\n' + indent(self.th)

class IfElse:
    def __init__(self, cond, th, el):
        self.cond, self.th, self.el = cond, th, el
    def __str__(self):
        return 'ifelse\n' + indent(self.cond) + '\n' + indent(self.th) + \
               '\n' + indent(self.el)

class While:
    def __init__(self, cond, bd):
        self.cond, self.bd = cond, bd
    def __str__(self):
        return 'while\n' + indent(self.cond) + '\n' + indent(self.bd)

class ArrayIndexing:
    def __init__(self, arr, ind):
        self.tp, self.arr, self.ind = arr.tp.base, arr, ind
    def __str__(self):
        return str(self.arr) + '[]\n '+ indent(self.ind)

class FieldSelection:
    def __init__(self, rec, fld):
        self.tp, self.rec, self.fld = fld.tp, rec, fld
#        self.tp, self.rec, self.fld = rec.tp.fields[fld].tp, rec, fld
    def __str__(self):
        return str(self.rec) + '.' + str(self.fld)

# public functions

def genBool(b):
    b.size = 4; return b

def genInt(i):
    i.size = 4; return i

def genRec(r):
    """Assuming r is Record, determine fields offsets and the record size"""
    s = 0
    for f in r.fields:
        f.offset, s = s, s + f.tp.size
    r.size = s
    return r

def genArray(a):
    """Assuming r is Array, determine its size"""
    # adds size
    a.size = a.length * a.base.size
    return a

def genLocalVars(sc, start):
    pass

def genGlobalVars(sc, start):
    pass

def genProgStart():
    pass

def genProgEntry(ident):
    pass

def genProgExit(x):
    return x

def genProcStart():
    pass

def genFormalParams(sc):
    pass

def genProcEntry(ident, parsize, localsize):
    pass

def genProcExit(x, parsize, localsize):
    pass

def genSelect(x, f):
    # x.f, assuming f is ST.Field
    return FieldSelection(x, f)

def genIndex(x, y):
    # x[y], assuming x is ST.Var, x.tp is ST.Array, y is Const or Reg integer
    return ArrayIndexing(x, y)

def genVar(x):
    # assuming x is ST.Var, ST.Ref, ST.Const
    return x

def genConst(x):
    return x

def genUnaryOp(op, x):
    return UnaryOp(op, x)

def genBinaryOp(op, x, y):
    return BinaryOp(op, x.tp, x, y)

def genRelation(op, x, y):
    return BinaryOp(op, Bool, x, y)

def genAssign(x, y):
    return Assignment(x, y)

def genActualPara(ap, fp, n):
    return ap

def genCall(x, ap):
    return Call(x.name, ap)

def genRead(x):
    return Call('read', [x])

def genWrite(x):
    return Call('write', [x])

def genWriteln():
    return Call('writeln', [])

def genSeq(x, y):
    return Seq(x, y)

def genCond(x):
    return x

def genIfThen(x, y):
    return IfThen(x, y)

def genThen(x, y):
    return y

def genIfElse(x, y, z):
    return IfElse(x, y, z)

def genTarget():
    pass

def genWhile(t, x, y):
    return While(x, y)

