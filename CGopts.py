# coding: utf-8

# ### P0 Code Generator for MIPS
# #### Emil Sekerinski, February 2017
# 
# The generated code is kept in memory and all code generation procedures 
# continuously append to that code: procedure `genProgStart` initializes the 
# generator, then `gen`-prefixed procedures are to be called for P0 constructs 
# as in order in which they are recognized by a recursive descent parser, and 
# finally procedure `genProgExit` returns the generated code in assembly 
# language as a string in a format that can be read in by the SPIM simulator. 
# The generation procedures are:
# - `genBool`, `genInt`, `genRec`, `genArray`
# - `genProgStart`, `genGlobalVars`, `genProgEntry`, `genProgExit`
# - `genProcStart`, `genFormalParams`, `genLocalVars`, `genProcEntry`, `genProcExit`
# - `genSelect`, `genIndex`, `genVar`, `genConst`, `genUnaryOp`, `genBinaryOp`, `genRelation`
# - `genAssign`, `genActualPara`, `genCall`, `genRead`, `genWrite`, `genWriteln`
# - `genSeq`, `genCond`, `genIfThen`, `genThen`, `genIfElse`, `genTarget`, `genWhile`
# 
# Errors in the code generator are reported by calling `mark` of the scanner. 
# The data types of the symbol table are used to specify the P0 constructs for 
# which code is to be generated.
from SC import TIMES, DIV, MOD, AND, PLUS, MINUS, OR, EQ, NE, LT, GT, LE, \
     GE, NOT, TILDE, AMP, BAR, mark
from ST import Var, Ref, Const, Type, Proc, StdProc, Int, Bool

# Following variables determine the state of the code generator:
# 
# - `curlev` is the current level of nesting of P0 procedures
# - `regs` is the set of available MIPS registers for expression evaluation
# - `label` is a counter for generating new labels
# - `declarations` is a list of list of declared variables (from the .data section)
#   Each list is a list of declared variables per scope
#   Each inner list consists of:
#   - name of the declared variable
#   - size of the declared variable
# - `instructions` is a list of lists of triples; 
#   each list is a list of triples for the current scope
#   each triple consists of three (possibly empty) strings:
#   - a label
#   - an instruction, possibly with operands
#   - a target (for branch and jump instructions)
# - `compiledProcedures` is a list of post-optimization procedure mips instructions
#   these will be inserted into the program after we're done

# Procedure `genProgStart()` initializes these variables. Registers `$t0` to 
# `$t9` are used as general-purpose registers.
def genProgStart():
    global declarations, instructions, compiledProcedures, procNames, fieldVars, usedGlobalVars
    global curlev, label, vname, regs, writtenText
    declarations = [ [] ]
    instructions = [ [] ]
    compiledProcedures = []
    procNames = []
    fieldVars = [ {} ]
    usedGlobalVars  = {}
    curlev = 0
    label = 0
    vname = 0
    regs = {'$t0', '$t1', '$t2', '$t3', '$t4', '$t5', '$t6', '$t7', '$t8'}
    writtenText = False
    putInstr('.data')

# Reserved registers are `$0` for the constant `0`, `$fp` for the frame pointer, 
# `$sp` for the stack pointer, and `$ra` for the return address (dynamic link).
R0 = '$0'; FP = '$fp'; SP = '$sp'; LNK = '$ra'

def obtainReg():
    if len(regs) == 0: mark('out of registers'); return R0
    else: return regs.pop()

def releaseReg(r):
    if r not in (R0, SP, FP, LNK): regs.add(r)

def putLab(lab, instr = ''):
    """Emit label lab with optional instruction; lab may be a single
    label or a list of labels"""
    if type(lab) == list:
        for l in lab[:-1]: instructions[-1].append((l, '', ''))
        instructions[-1].append((lab[-1], instr, ''))
    else: instructions[-1].append((lab, instr, ''))

def putInstr(instr, target = ''):
    """Emit an instruction"""
    instructions[-1].append(('', instr, target))

def putOp(op, a, b, c):
    """Emit instruction op with three operands, a, b, c; c can be register or immediate"""
    putInstr(op + ' ' + a + ', ' + b + ', ' + str(c))

def putBranchOp(op, a, b, c):
    putInstr(op + ' ' + a + ', ' + b, str(c))

def putMemOp(op, a, b, c):
    """Emit load/store instruction at location or register b + offset c"""
    if b == R0: putInstr(op + ' ' + a + ', ' + str(c))
    else: putInstr(op + ' ' + a + ', ' + str(c) + '(' + b + ')')

# Following procedures "generate code" for all P0 types by determining the size 
# of objects and store in the `size` field.
# - Integers and booleans occupy 4 bytes
# - The size of a record is the sum of the sizes of its field; the offset of a 
#   field is the sum of the size of the preceding fields
# - The size of an array is its length times the size of the base type.
def genBool(b):
    b.size = 4; return b

def genInt(i):
    i.size = 4; return i

def genRec(r):
    # r is Record
    s = 0
    for f in r.fields:
        f.offset, s = s, s + f.tp.size
    r.size = s
    return r

def genArray(a):
    # a is Array
    a.size = a.length * a.base.size
    return a

# For each global variable, `genGlobalVars(sc, start)` adds the identifier
# to the list of global declarations and marks it as unused
# The parameter `sc` contains the top scope  with all declarations parsed so far; 
# only variable declarations from index `start` on in the top scope are considered. 
# As MIPS instructions are not allowed to be identifiers, all variables get `_` 
# as suffix to avoid a name clash.
def genGlobalVars(sc, start):
    for i in range(len(sc) - 1, start - 1, - 1):
        if type(sc[i]) == Var:
            sc[i].adr = sc[i].name + '_'
            declarations[curlev].append( (sc[i].adr, sc[i].tp.size) )
            usedGlobalVars[sc[i].adr] = False

# Procedure `genProgEntry(ident)` takes the program's name as a parameter. 
# Directives for marking the beginning of the main program are generated; the 
# program's name is it not used. 
def genProgEntry(ident):
    putInstr('.globl main')
    putInstr('.ent main')
    putLab('main')

# Procedure `genProgExit(x)` takes parameter `x` with the result of previous 
# `gen-` calls, generates code for exiting the program, directives for marking 
# the end of the main program, and returns the complete assembly code.
def assembly(l, i, t):
    """Convert label l, instruction i, target t to assembly format"""
    return (l + ':\t' if l else '\t') + i + (', ' + t if t else '')

def genProgExit(x):
    putInstr('li $v0, 10')
    putInstr('syscall')
    putInstr('.end main')
    runOptimizations()
    combineInstructions()
    return '\n'.join(assembly(l, i, t) for (l, i, t) in instructions[-1])

def combineInstructions():
    prelude = [instructions[curlev][0]] + declarations[curlev] + [('', '.text', '')]
    for proc in compiledProcedures:
        prelude += proc
    prelude += instructions[curlev][1:]
    instructions[curlev] = prelude

# BEGIN OPTIMIZATIONS
def runOptimizations():
    removeUnusedProcedures()
    # deadStoreElimination()
    removeUnusedVariables(genDataDecl)

# Searches for 'jal' in main, indicating a procedure is being used in the main loop
def removeUnusedProcedures():
    global instructions, compiledProcedures
    currentInstructions = instructions[curlev]
    procedureNames = []
    temp = []
    for i in range(len(currentInstructions)):
        (l,ins,target) = currentInstructions[i]   
        if (ins == 'jal'):
            recursiveProcedureSearch(procedureNames, temp, target)
    compiledProcedures = temp

# Recursive search for 'jal' in current procedure, indicating a procedure is being used within another procedure, which then searches 
def recursiveProcedureSearch(procedureNames, temp, target):
    for procedure in range(len(compiledProcedures)):
        searchCurrentProcedure = False
        for line in range(len(compiledProcedures[procedure])):
            if (line == 2 and compiledProcedures[procedure][line][0] == target):
                if compiledProcedures[procedure][line][0] not in procedureNames:
                    procedureNames.append(compiledProcedures[procedure][line][0])
                    temp.append(compiledProcedures[procedure])
                    searchCurrentProcedure = True
            if (searchCurrentProcedure and compiledProcedures[procedure][line][1] == 'jal'):
                recursiveProcedureSearch(procedureNames, temp, compiledProcedures[procedure][line][2])

# REMOVE UNUSED VARIABLES
def removeUnusedVariables(f):
    global declarations, usedGlobalVars
    currentInstructions = instructions[curlev]
    temp = []
    completed = []
    realAddrs = {}
    s = 0
    for i in range(len(currentInstructions)):
        (l,ins,target) = currentInstructions[i]    
        for decl in declarations[curlev]:
            name = decl[0]
            size = decl[1]
            if name in completed: continue
            if name in usedGlobalVars or name in ins or (type(target) == str and name in target):
                completed.append(name)
                s = s + size
                realAddrs[name] = -s - 8
                if name not in usedGlobalVars:
                    temp.append( f(decl) )
                elif not usedGlobalVars[name]:
                    usedGlobalVars[name] = True
    declarations[curlev] = temp
    return realAddrs,s

def genDataDecl(decl):
    return (decl[0], '.space ' + str(decl[1]), '')

def genProcDecl(decl):
    return decl

def processProcInstructions(realAddrs, size):
    global instructions, procNames
    currentInstructions = instructions[curlev]
    procName = '__LOCAL_' + procNames[-1]
    localsize = str(size + 8)
    for i in range(len(currentInstructions)):
        (l,ins,target) = currentInstructions[i]
        if procName in ins:
            ins = ins.replace(procName, localsize)
        for varAddr in realAddrs:
            if type(varAddr) != str:
                continue
            varSize = str(realAddrs[varAddr])            
            if varAddr in ins:
                ins = ins.replace(varAddr, varSize)
            if type(target) == str and varAddr in target:
                target = target.replace(varAddr, varSize)
        currentInstructions[i] = (l, ins, target)
    instructions[curlev] = currentInstructions

# DEAD STORE ELIMINATION
def deadStoreElimination():
    global compiledProcedures
    instructions[curlev] = deadStoreEliminationFromBlock(instructions[curlev])
    newCompiledProcedures = []
    for compiledProc in compiledProcedures:
        newCompiledProcedures.append(deadStoreEliminationFromBlock(compiledProc))
    compiledProcedures = newCompiledProcedures

# Dead store elimination looks for `sw` instructions not followed
# by a `lw` instruction from the same reg
def deadStoreEliminationFromBlock(block):
    readAddresses = {}
    result = []
    for elem in block[::-1]:
        (_,ins,_) = elem
        # assuming all targets for branch/jump instructions are labels
        if ins.startswith('lw'):
            # this is a read
            t,reg,loc = ins.split()
            readAddresses[loc] = True
            result.append(elem)
        elif ins.startswith('sw'):
            # this is a write
            t,reg,loc = ins.split()
            # if we're writing here and we've read from it before, keep the instruction
            # otherwise we discard it
            if loc in readAddresses and readAddresses[loc]:
                result.append(elem)
                readAddresses[loc] = False
        else:
            result.append(elem)
    return result[::-1]

'''
Common subexpression elimination:
The basic logic for this area is as follows:

Create a list of expressions, initially empty.
Go through the current block and record every instruction, the format is:

subexpressions = [[Instruction, [lines]], [Instruction2, [lines]], [Instruction3, [lines]], ...]

(this will eventually only grab relevant ones, lw, addi, and such)

This only records if there is no duplicate line, if there is the line is instead appended to the relevant lines list in the set.

At the same time, each used register used is recorded in a different list in a similar format.

used_registers = [[Register, [lines]], [Register2, [lines]], [Register3, [lines]], ...]

Quickly check that there is at least 1 entry in subexpressions with more than 1 entry in [lines], otherwise there are no
common subexpressions and the optimization is redundant.  Any subexpression with a len([lines]) = 1 is removed from the list.

Not relevant for this particular compiler, but an important note:
Go through the block and check for any instances of data being stored to memory then it being loaded, this is an indication
that there are not enough registers for the function of this block, and common subexpression elimination will not assist as it
performs best when replacing them with register stored values, not from memory.  In fact, doing so can slow the program down in
some cases.  If this occurs, the optimization does not continue.

The list of registers is crucial as it will determine what registers are free to use, what registers can be used at some points
and not others, and others still that are used throughout and cannot be modified.  Since this all occurs after smart registry
use optimization, we can assume that the range of lines between occurances are considered "blocked" for use.  Effectively, this
means that the first and last occurance is blocked off and we cannot use that register for any other purpose during those lines.
But before and after the blocked section is fair game.

This list of subexpressions is then sorted based on the len of the [lines] list for each instruction, with priority being given
to those with the most (i.e. the most common subexpressions).  This ordering is important as it ensures that the best optimizations
are taken in case of there being more CSEs than usable registers.

Now for each CSE:
    Now we must check if the CSE is actually valid, and that no changes to the used variables occured.  For instance:

    a = x + y
    b = x + y

    x + y is a valid CSE.

    a = x + y + i
    b = x + y + j

    x + y is also a valid CSE.  However, instances can occur where:

    a = x + y + i
    ...
    x = x + 6
    y = 20 + var
    ...

    b = x + y + j

    x + y is NOT a valid CSE, as the value of x and y has changed.

    This is important to


    The line number range from the first and last occurance is determined, this is used to determine what register should be used.
    priority is given to those with the "best fit", the registers that are free for the shortest period before and after
    (but still free throughout) this range. This allows for other CSEs to potentially use registers that we otherwise
    may have blocked off.  If no used registers exist that are free, an usused register is used if it exists.  If none do, the
    CSE is skipped and the next CSE proceeds.
'''
#Reuse registers if possible, count from bottom!
#(label, instruction, target)


# Procedure `newLabel()` generates a new unique label on each call.
def newLabel():
    global label
    label += 1
    return 'L' + str(label)

# Procedure `newVarName()` generates a new unique variable placeholder name on each call.
def newVarName():
    global vname
    vname += 1
    return '__V' + str(vname)

# The code generator _delays the generation of code_ until it is clear that no 
# better code can be generated. For this, the not-yet-generated result of an 
# expressions and the location of a variable is stored in _items_. In addition 
# to the symbol table types `Var`, `Ref`, `Const`, the generator uses two more 
# item types:
# - `Reg(tp, reg)` for integers or boolean values stored in a register; the 
#    register can be `$0` for constants `0` and `false`
# - `Cond(cond, left, right)` for short-circuited Boolean expressions with two 
#   branch targets. The relation `cond` must be one of `'EQ'`, `'NE'`, `'LT'`, 
#   `'GT'`, `'LE'`, `'GE'`. The operands `left`, `right` are either registers or 
#   constants, but one has to be a register. The result of the comparison is 
#   represented by two branch targets, stored as fields, where the evaluation 
#   continues if the result of the comparison is true or false. The branch 
#   targets are lists of unique labels, with targets in each list denoting the 
#   same location. If `right` is `$0`, then `'EQ'` and `'NE'` for `cond` can be 
#   used for branching depending on whether `left` is `true` or `false`.
class Reg:
    def __init__(self, tp, reg):
        # tp is Bool or Int
        self.tp, self.reg = tp, reg

class Cond:
    # labA, labB are lists of branch targets for when the result is true or false
    def __init__(self, cond, left, right):
        self.tp, self.cond, self.left, self.right = Bool, cond, left, right
        self.labA, self.labB = [newLabel()], [newLabel()]

# Procedure `loadItemReg(x, r)` generates code for loading item `x` to register 
# `r`, assuming `x` is `Var`, `Const`, or `Reg`. If a constant is too large to 
# fit in 16 bits immediate addressing, an error message is generated.
# If we're loading a variable, mark it as being used
def testRange(x):
    if x.val >= 0x8000 or x.val < -0x8000: mark('value too large')

def loadItemReg(x, r):
    if type(x) == Var: 
        putMemOp('lw', r, x.reg, x.adr)
        releaseReg(x.reg)
    elif type(x) == Const:
        testRange(x); putOp('addi', r, R0, x.val)
    elif type(x) == Reg: # move to register r
        putOp('add', r, x.reg, R0)
    else: assert False

# Procedure `loadItem(x)` generates code for loading item `x`, which has to be 
# `Var` or `Const`, into a new register and returns a `Reg` item; if `x` is 
# `Const` and has value `0`, no code is generated and register `R0` is used 
# instead. For procedure `loadBool(x)`, the type of item `x` has to be `Bool`; 
# if `x` is not a constant, it is loaded into a register and a new `Cond` item 
# is returned.
def loadItem(x):
    if type(x) == Const and x.val == 0: r = R0 # use R0 for "0"
    else: r = obtainReg(); loadItemReg(x, r)
    return Reg(x.tp, r)

def loadBool(x):
    if type(x) == Const and x.val == 0: r = R0 # use R0 for "false"
    else: r = obtainReg(); loadItemReg(x, r)
    return Cond(NE, r, R0)

# Procedure `put(cd, x, y)` generates code for `x op y`, where `op` is an 
# operation with mnemonic `cd`. Items `x`, `y` have to be `Var`, `Const`, `Reg`.
# An updated item `x` is returned.
def put(cd, x, y):
    if type(x) != Reg: x = loadItem(x)
    if x.reg == R0: x.reg, r = obtainReg(), R0
    else: r = x.reg # r is source, x.reg is destination
    if type(y) == Const:
        testRange(y); putOp(cd, r, x.reg, y.val)
    else:
        if type(y) != Reg: y = loadItem(y)
        putOp(cd, x.reg, r, y.reg); releaseReg(y.reg)
    return x

# Procedures `genVar`, `genConst`, `genUnaryOp`, `genBinaryOp`, `genRelation`, 
# `genSelect`, and `genIndex` generate code for expressions (e.g. right hand 
# side of assignments) and for  locations (e.g. left hand side of assignments).
# 
# Procedure `genVar(x)` allows `x` to refer to a global variable, local variable, 
# or procedure parameter. References to variables on intermediate level is not 
# supported. Local variables and procedure parameters are addressed FP-relative. 
# For global variables, the reference is kept symbolic, to be resolved later by 
# the assembler. Item `x` is `Var` or `Ref`; if it is `Ref`, the reference is 
# loaded into a new register. A new `Var` item with the location is returned.
def genVar(x):
    if x.lev == 0: s = R0 # global variable at x.adr
    elif x.lev == curlev: s = FP # local variable, FP relative
    else: mark('level!'); s = R0
    y = Var(x.tp); y.lev = x.lev
    if type(x) == Ref: # reference is loaded into register
        r = obtainReg(); putMemOp('lw', r, s, x.adr)
        y.reg, y.adr = r, 0 # variable at (y.reg)
    elif type(x) == Var:
        y.reg, y.adr = s, x.adr
    else: assert False
    return y

# Procedure `genConst(x)` does not need to generate any code.
def genConst(x):
    # x is Const
    return x

# Procedure `genUnaryOp(op, x)` generates code for `op x` if `op` is `MINUS`, 
# `NOT` and `x` is `Int`, `Bool`; if `op` is `AND`, `OR`, item `x` is the first 
# operand. If it is not already a `Cond` item, it is made so which is loaded 
# into a register. A branch instruction is generated for `OR` and a branch 
# instruction with a negated condition for `AND`.
def negate(cd):
    return {EQ: NE, NE: EQ, LT: GE, LE: GT, GT: LE, GE: LT}[cd]

def condOp(cd):
    return {EQ: 'beq', NE: 'bne', LT: 'blt', LE: 'ble', GT: 'bgt', GE: 'bge'}[cd]

def genUnaryOp(op, x):
    if op == MINUS: # subtract from 0
        if type(x) == Var: x = loadItem(x)
        putOp('sub', x.reg, R0, x.reg)
    elif op == TILDE: # `r nor s` means ~(r | s) so we use `r nor r` to get ~r
        if type(x) == Var: x = loadItem(x)
        putOp('nor', x.reg, x.reg, x.reg)
    elif op == NOT: # switch condition and branch targets, no code
        if type(x) != Cond: x = loadBool(x)
        x.cond = negate(x.cond); x.labA, x.labB = x.labB, x.labA
    elif op == AND: # load first operand into register and branch
        if type(x) != Cond: x = loadBool(x)
        putBranchOp(condOp(negate(x.cond)), x.left, x.right, x.labA[0])
        releaseReg(x.left); releaseReg(x.right); putLab(x.labB)
    elif op == OR: # load first operand into register and branch
        if type(x) != Cond: x = loadBool(x)
        putBranchOp(condOp(x.cond), x.left, x.right, x.labB[0])
        releaseReg(x.left); releaseReg(x.right); putLab(x.labA)
    else: assert False
    return x

# Procedure `genBinaryOp(op, x, y)` generates code for `x op y` if `op` is 
# `PLUS`, `MINUS`, `TIMES`, `DIV`, `MOD`. If `op` is `AND`, `OR`, operand `y` is
# made a `Cond` item it if is not so already and the branch targets are merged. 
def genBinaryOp(op, x, y):
    if op == PLUS: y = put('add', x, y)
    elif op == MINUS: y = put('sub', x, y)
    elif op == TIMES: y = put('mul', x, y)
    elif op == DIV: y = put('div', x, y)
    elif op == MOD: y = put('mod', x, y)
    elif op == AND: # load second operand into register 
        if type(y) != Cond: y = loadBool(y)
        y.labA += x.labA # update branch targets
    elif op == OR: # load second operand into register
        if type(y) != Cond: y = loadBool(y)
        y.labB += x.labB # update branch targets
    elif op == AMP: y = put('and', x, y)
    elif op == BAR: y = put('or', x, y)
    else: assert False
    return y

# Procedure `genRelation(op, x, y)` generates code for `x op y` if `op` is `EQ`,
# `NE`, `LT`, `LE`, `GT`, `GE`. Items `x` and `y` cannot be both constants. A 
# new `Cond` item is returned.
def genRelation(op, x, y):
    if type(x) != Reg: x = loadItem(x)
    if type(y) != Reg: y = loadItem(y)
    return Cond(op, x.reg, y.reg)

# Procedure `genSelect(x, f)` "generates code" for `x.f`, provided `f` is in 
# `x.fields`. Only `x.adr` is updated, no code is generated. An updated item is 
# returned.
# In essence we're treating `x.f` as an entirely different variable than
# any other element in `x`. In practice they will typically end up next to each
# other in memory.
# However with these "optimizations" we don't guarantee that all fields in a record
# will occupy a contiguous memory space
def genSelect(x, f):
    global fieldVars
    x.tp = f.tp
    if type(x.adr) == int:
        x.adr = x.adr + f.offset
    else:
        # lookup the field name to see if we've already seen it
        fieldName = '__' + str(x) + '.' + str(f)
        if fieldName not in fieldVars[curlev]:
            tempName = newVarName()
            declarations[curlev].append( (tempName, f.tp.size) )
            fieldVars[curlev][fieldName] = tempName
            x.adr = tempName
        else:
            x.adr = fieldVars[curlev][fieldName]
    return x

# Procedure `genIndex(x, y)` generates code for `x[y]`, assuming `x` is `Var` or
# `Ref`, `x.tp` is `Array`, and `y.tp` is `Int`. If `y` is `Const`, only `x.adr`
# is updated and no code is generated, otherwise code for array index 
# calculation is generated.
# TODO figure out the best way to do this while also preserving pointer semantics
def genIndex(x, y):
    if type(y) == Const:
        offset = (y.val - x.tp.lower) * x.tp.base.size
        x.adr = x.adr + (offset if type(x.adr) == int else '+' + str(offset))
    else:
        if type(y) != Reg: y = loadItem(y)
        putOp('sub', y.reg, y.reg, x.tp.lower)
        putOp('mul', y.reg, y.reg, x.tp.base.size)
        if x.reg != R0:
            putOp('add', y.reg, x.reg, y.reg); releaseReg(x.reg)
        x.reg = y.reg
    x.tp = x.tp.base
    return x

# Procedure `genAssign(x, y)` generates code for `x := y`, provided `x` is 
# `Var`. Item `x` is loaded into a register if it is not already there; if `x` 
# is `Cond`, then either `0` or `1` is loaded into a register.
# Mark variable `x` as being used.
def genAssign(x, y):
    """Assume x is Var, generate x := y"""
    if type(y) == Cond:
        putBranchOp(condOp(negate(y.cond)), y.left, y.right, y.labA[0])
        releaseReg(y.left); releaseReg(y.right); r = obtainReg()
        putLab(y.labB); putOp('addi', r, R0, 1) # load true
        lab = newLabel()
        putInstr('b', lab)
        putLab(y.labA); putOp('addi', r, R0, 0) # load false 
        putLab(lab)
    elif type(y) != Reg: y = loadItem(y); r = y.reg
    else: r = y.reg
    putMemOp('sw', r, x.reg, x.adr); releaseReg(r)


# The procedure calling convention is as follows:
# - last parameter at `0($fp)`, 2nd last at `4($fp)`, ...
# - previous frame pointer at `-4($fp)`
# - return address at `-8($fp)`
# - 1st local at `-12($fp)`, ...
# 
# The Stack pointer `$sp` points to last used location on the stack.
# 
# On procedure entry:
# - caller pushes 1st parameter at `-4($sp)`, 2nd at `-8($sp)`, ...
# - caller calls callee
# - callee saves `$fp` at `$sp - parameter size - 4`
# - callee saves `$ra` at `$sp - parameter size - 8`
# - callee sets `$fp` to `$sp - parameter size`
# - callee sets `$sp` to `$fp - local var size - 8`
# 
# On procedure exit:
# - callee sets `$sp` to `$fp + parameter size`
# - callee loads `$ra` from `$fp - 8`
# - callee loads `$fp` from `$fp - 4`
# - callee returns
# 
# For each local variable, `genLocalVars(sc, start)` updates the entry of the 
# variable with the FP-relative address and returns their total size. The 
# parameter `sc` contains the top scope with all local declarations parsed so 
# far; only variable declarations from index `start` on in the top scope are 
# considered.
def genLocalVars(sc, start):
    s = 0 # local block size
    for i in range(start, len(sc)):
        if type(sc[i]) == Var:
            s = s + sc[i].tp.size
            sc[i].adr = newVarName() # replace the address with a random name
                                     # compute the real address at the end
            name = str(sc[i].adr)
            declarations[curlev].append( (name, sc[i].tp.size) )
    return s

# Procedure `genProcStart()` adds a new scope to the declarations, instructions global variables
def genProcStart():
    global curlev, declarations, instructions
    declarations.append( [] )
    instructions.append( [] )
    fieldVars.append( {} )
    curlev = curlev + 1


# Procedure `genFormalParams(sc)` determines the FP-relative address of all 
# parameters in the list `sc` of procedure parameters. Each parameter must be of
# type `Int` or `Bool` or must be a reference parameter.
def genFormalParams(sc):
    s = 0 # parameter block size
    for p in reversed(sc):
        if p.tp == Int or p.tp == Bool or type(p) == Ref:
            p.adr, s = s, s + 4
        else: mark('no structured value parameters')
    return s

# Procedures `genProcEntry(ident, parsize, localsize)` and 
# `genProcExit(x, parsize, localsize)` generate the procedure prologue and epilogue.
def genProcEntry(ident, parsize, localsize):
    putInstr('.globl ' + ident)              # global declaration directive
    putInstr('.ent ' + ident)                # entry point directive
    putLab(ident)                            # procedure entry label
    putMemOp('sw', FP, SP, - parsize - 4)    # push frame pointer
    putMemOp('sw', LNK, SP, - parsize - 8)   # push return address
    putOp('sub', FP, SP, parsize)            # set frame pointer
    putOp('sub', SP, FP, '__LOCAL_' + ident) # set stack pointer to temp label, 
                                             # which we replace and compute at the end
    procNames.append(ident)

def genProcExit(x, parsize, localsize):
    global curlev, declarations, instructions, compiledProcedures
    
    putOp('add', SP, FP, parsize) # restore stack pointer
    putMemOp('lw', LNK, FP, - 8)  # pop return address
    putMemOp('lw', FP, FP, - 4)   # pop frame pointer
    putInstr('jr $ra')            # return
    
    # apply optimizations
    realAddresses,localsize = removeUnusedVariables(genProcDecl)
    processProcInstructions(realAddresses, localsize)

    # remove declarations, instructions for the procedure we're exiting
    procDecl = declarations.pop()
    procIns = instructions.pop()
    procNames.pop()
    fieldVars.pop()
    curlev = curlev - 1
    # store the compiled instructions in compiledProcedures
    compiledProcedures.append(procIns)

# Procedure `genActualPara(ap, fp, n)` assume that `ap` is an item with the 
# actual parameter, `fp` is the entry for the formal parameter, and `n` is the 
# parameter number. The parameters are pushed SP-relative on the stack. The 
# formal parameter is either `Var` or `Ref`.
def genActualPara(ap, fp, n):
    if type(fp) == Ref:  #  reference parameter, assume p is Var
        if ap.adr != 0:  #  load address in register
            r = obtainReg(); putMemOp('la', r, ap.reg, ap.adr)
        else: r = ap.reg  #  address already in register
        putMemOp('sw', r, SP, - 4 * (n + 1)); releaseReg(r)
    else:  #  value parameter
        if type(ap) != Cond:
            if type(ap) != Reg: ap = loadItem(ap)
            putMemOp('sw', ap.reg, SP, - 4 * (n + 1)); releaseReg(ap.reg)
        else: mark('unsupported parameter type')

# Procedure `genCall(pr, ap)` assumes `pr` is `Proc` and `ap` is a list of actual parameters.
def genCall(pr, ap):
    putInstr('jal', pr.name)

# Procedures `genRead(x)`, `genWrite(x)`, `genWriteln()` generate code for 
# SPIM-defined "syscalls"; `genRead(x)` and assumes that `x` is `Var` and 
# `genWrite(x)` assumes that `x` is `Ref`, `Var`, `Reg`.  
def genRead(x):
    putInstr('li $v0, 5'); putInstr('syscall')
    putMemOp('sw', '$v0', x.reg, x.adr)

def genWrite(x):
    loadItemReg(x, '$a0'); putInstr('li $v0, 1'); putInstr('syscall')

def genWriteln():
    putInstr('li $v0, 11'); putInstr("li $a0, '\\n'"); putInstr('syscall')

# For control structures:
# - `genSeq(x, y)` generates `x ; y`, assuming `x`, `y` are statements
# - `genCond(x)` generates code for branching on `x`, assuming that `x` is of type `Bool`
# - `genIfThen(x, y)` generates code for `y` in `if x then y`
# - `genThen(x, y)` generates code for `y` in `if x then y else z`
# - `genIfElse(x, y, z)` generates code for `z` in `if x then y else z`
# - `genTarget()` generates and returns a target for backward branches
# - `genWhile(lab, x, y)` generates code for `y` in `while x do y`, assuming that target `lab` was generated before `x`.
def genSeq(x, y):
    pass

def genCond(x):
    if type(x) != Cond: x = loadBool(x)
    putBranchOp(condOp(negate(x.cond)), x.left, x.right, x.labA[0])
    releaseReg(x.left); releaseReg(x.right); putLab(x.labB)
    return x

def genIfThen(x, y):
    putLab(x.labA)

def genThen(x, y):
    lab = newLabel()
    putInstr('b', lab)
    putLab(x.labA); 
    return lab

def genIfElse(x, y, z):
    putLab(y)

def genTarget():
    lab = newLabel()
    putLab(lab)
    return lab

def genWhile(lab, x, y):
    putInstr('b', lab)
    putLab(x.labA); 

