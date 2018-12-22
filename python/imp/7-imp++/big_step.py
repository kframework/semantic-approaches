from abc import *
import sys
from enum import Enum
import random 

def debug(func):
	def wrapper_inner(*arg, **kwargs):
		print("Debug....")
		print(type(func), arg)
		return func(*arg, **kwargs)
	return wrapper_inner

class StateType(Enum):
	ERROR = 1
	HALTING = 2
	EPSILON = 3

class Op(Enum):
	PLUS = 1
	DIVIDE = 2
	AND = 3
	LEQ = 4

class Configuration(ABC):
	@abstractmethod
	def execute(self, state, buf): pass

class ConfExp(Configuration):
	def __init__(self, value, state, buf):
		self.value = value
		self.state = state
		self.buf = buf

	def execute(self, state, buf):pass

class ConfState(Configuration):
	def __init__(self, state, bufin, bufout):
		self.state = state
		self.bufin = bufin
		self.bufout = bufout

	def execute(self, state, buf):pass

	def printf(self):
		for i in self.state:
			print(i, self.state[i])
		print(self.bufin)
		print(self.bufout)

class ConfHalting(Configuration):
	def __init__(self, state, bufin, bufout):
		self.state = state
		self.bufin = bufin
		self.bufout = bufout

	def execute(self, state, buf):pass

class Pgm(Configuration):
	def __init__(self, _vars, stmt, buf):
		self.vars = _vars
		self.stmt = stmt
		self.buf = buf

	def execute(self):
		state = {}
		for var in self.vars:
			if var not in state:
				state[var] = 0

		return self.stmt.execute(state, self.buf)

# Stmt
class StmtLet(Configuration):
	def __init__(self, _id, aexp, stmt):
		self.id = _id
		self.aexp = aexp
		self.stmt = stmt

	def execute(self, state, buf):
		i, state1, buf1 = self.aexp.execute(state, buf)
		if i == StateType.ERROR:
			return ConfHalting(state1, buf1, StateType.EPSILON)

		if self.id not in state:
			print(self.id, "not defined!")
			sys.exit()

		state[self.id] = i
		return self.stmt.execute(state, buf)


class StmtSeq(Configuration):
	def __init__(self, stmt1, stmt2):
		self.stmt1 = stmt1
		self.stmt2 = stmt2

	@debug
	def execute(self, state, buf):
		if type(self.stmt1) == StmtSpawn:
			i = random.randint(0,1)
			if i == 1:
				stmt = self.stmt1
				self.stmt1 = self.stmt2
				self.stmt2 = stmt

		state1 = self.stmt1.execute(state,buf)
		if type(state1) == ConfHalting:
			return state1
		state2 = self.stmt2.execute(state1.state, state1.bufin)
		bufout = []
		if state1.bufout != StateType.EPSILON:
			bufout.execute(state1.bufout)
		if state2.bufout != StateType.EPSILON:
			bufout.execute(state2.bufout)
		if len(bufout) == 0:
			bufout = StateType.EPSILON

		if type(state2) == ConfHalting:
			return ConfHalting(state2.state, state2.bufin,bufout)
		return ConfState(state2.state, state2.bufin, bufout)

class StmtIf(Configuration):
	def __init__(self, bexp, stmt1, stmt2):
		self.bexp = bexp
		self.stmt1 = stmt1
		self.stmt2 = stmt2

	def execute(self, state, buf):
		b, state1, buf1 = self.bexp.execute(state, buf)
		if b == StateType.ERROR:
			return ConfHalting(state1, buf1, StateType.EPSILON)
		if b:
			return stmt1.execute(state1, buf1)
		if not b:
			return stmt2.execute(state1, buf1)

class StmtWhile(Configuration):
	def __init__(self, bexp, stmt):
		self.bexp = bexp
		self.stmt = stmt

	def execute(self, state, buf):
		b, state1, buf1 = self.bexp.execute(state, buf)
		if b == StateType.ERROR:
			return ConfHalting(state1, buf1, StateType.EPSILON)

		if not b:
			return ConfState(state1, buf1, StateType.EPSILON)

		stmt = StmtSeq(self.stmt, StmtWhile(self.bexp, self.stmt))
		return stmt.execute(state1, buf1)
		
class StmtBlock(Configuration):
	def __init__(self, stmt):
		self.stmt = stmt

	def execute(self, state, buf):
		return stmt.execute(state, buf)

class StmtAssign(Configuration):
	def __init__(self, _id, exp): 
		self.id = _id
		self.exp = exp

	@debug
	def execute(self, state, buf):
		state1 = self.exp.execute(state, buf)
		if state1.value != StateType.ERROR:
			print(state1.state)
			if self.id not in state1.state:
				print("At StmtAssign", self.id, "not defined!")
				sys.exit()
			print("haha")
			state1.state[self.id] = state1.value
			return ConfState(state1.state, state1.buf, StateType.EPSILON)
		else:
			return ConfHalting(state1.stae, StateType.EPSILON)

class StmtPrint(Configuration):
	def __init__(self, exp):
		self.exp = exp

	def execute(self, state, buf):
		i, state1, buf1 = self.exp.execute()
		if i == StateType.ERROR:
			return ConfHalting(state1, buf1, StateType.EPSILON)
		return ConfState(state1, buf1, i)

class StmtSpawn(Configuration):
	def __init__(self, stmt):
		self.stmt = stmt

	def execute(self, state, buf):
		return self.stmt.execute()

class StmtHalt(Configuration):
	def execute(self, state, buf):
		return ConfHalting(state, buf, StateType.EPSILON)

# BExp
class BExpNot(Configuration):
	def __init__(self, exp):
		self.exp = exp

	def execute(self, state, buf):
		config = self.exp.execute(state, buf)
		if config.value != StateType.ERROR:
			config.value = not config.value
		return config

class BExpOp(Configuration):
	def __init__(self, exp1, exp2, op):
		self.exp1 = exp1
		self.exp2 = exp2
		self.op = op

	def execute(self, state, buf):
		i = random.randint(0,1)
		if i == 1:
			conf1 = self.exp1.execute(state,buf)
		else:
			conf1 = self.exp2.execute(state,buf)

		# if b1 is error, or b1 is false for B1 && B2
		if conf1.value == StateType.ERROR or (self.op == Op.AND and not conf1.value):
			return conf1

		if i == 1:
			conf2 = self.exp2.execute(conf2.state, conf2.buf)
		else:
			conf2 = self.exp1.execute(conf2.state, conf2.buf)
		if conf2.value == StateType.ERROR:
			return conf2

		if self.op == Op.LEQ:
			return ConfExp(conf1.value <= conf2.value, conf2.state, conf2.buf)
		if self.op == Op.AND:
			return ConfExp(conf1.value and conf2.value, conf2.state, conf2.buf)


class BExpConst(Configuration):
	def __init__(self, b):
		self.b = b

	def execute(self, state, buf):
		return ConfExp(b, state, buf)

# AExp
class AExpOp(Configuration):
	def __init__(self, op, aexp1, aexp2):
		self.op = op
		self.aexp1 = aexp1
		self.aexp2 = aexp2

	def execute(self, state, buf):
		# deal with non-deterministic
		i = random.randint(0,1)
		if i == 1:
			conf1 = self.aexp1.execute(state,buf)
		else:
			conf1 = self.aexp2.execute(state,buf)
		if conf1.value == StateType.ERROR:
			return conf1

		if i == 1:
			conf2 = self.aexp2.execute(conf1.state,conf1.buf)
		else:
			conf2 = self.aexp1.execute(conf1.state,conf1.buf)
		if conf2.value == StateType.ERROR:
			return conf2

		if self.op == Op.PLUS:
			return ConfExp(conf1.value+conf2.value, conf2.state, conf2.buf)
		if self.op == Op.DIVIDE:
			if conf2.value == 0:
				return ConfExp(StateType.ERROR, state2, buf2)
			else:
				return ConfExp(conf1.value/conf2.value, conf2.state, conf2.buf)

class AExpConstant(Configuration):
	def __init__(self, i):
		self.i = i

	def execute(self,state,buf):
		return ConfExp(self.i, state, buf)

class AExpId(Configuration):
	def __init__(self, _id):
		self.id = _id

	@debug
	def execute(self, state, buf):
		if self.id not in state:
			print("At AExpId", self.id, "not defined!")
			sys.exit()

		return ConfExp(state[self.id], state, buf)

class AExpPP(Configuration):
	def __init__(self, _id):
		self.id = _id

	@debug
	def execute(self, state, buf):
		if self.id not in state:
			print("At AExpPP", self.id, "not defined!")
			sys.exit()

		state[self.id] += 1
		return ConfExp(state[self.id]-1, state, buf)

class AExpRead(Configuration):
	def execute(self, state, buf):
		return ConfExp(buf.pop(0), state, buf)

