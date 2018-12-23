from abc import *
import sys
from enum import Enum
import random 
from util import *
import copy

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
		self.state = copy.deepcopy(state)
		self.buf = buf

	def execute(self, state, buf):pass

	def printf(self):
		print(self.value)
		for i in self.state:
			print(i, self.state[i])
		print(self.buf)

	def __hash__(self):
		buf = str(self.buf)
		#print(hash((self.value, buf, self.state.__hash__())))
		return hash((self.value, buf, self.state.__hash__()))

	def __eq__(self, other):
		return self.__hash__() == other.__hash__()


class ConfState(Configuration):
	def __init__(self, state, bufin, bufout):
		self.state = copy.deepcopy(state)
		self.bufin = bufin
		self.bufout = bufout

	def execute(self, state, buf):pass

	def printf(self):
		s = "skip\nstate: {"
		for i in self.state:
			s = s + str(i) + ": " + str(self.state[i]) + "; " 
		s = s + "} \nbufin: " + str(self.bufin) + " \nbufout: " + str(self.bufout)
		print(s)

	def __hash__(self):
		bufin = str(self.bufin)
		bufout = str(self.bufout)
		#print(hash(('True', bufout, bufin, self.state.__hash__())))
		return hash(('True', bufout, bufin, self.state.__hash__()))

	def __eq__(self, other):
		return self.__hash__() == other.__hash__()

class ConfHalting(Configuration):
	def __init__(self, state, bufin, bufout):
		self.state = copy.deepcopy(state)
		self.bufin = bufin
		self.bufout = bufout

	def execute(self, state, buf):pass

	def printf(self):
		s = "Halting... \nstate: {"
		for i in self.state:
			s = s + str(i) + ": " + str(self.state[i]) + "; " 
		s = s + "} \nbufin: " + str(self.bufin) + " \nbufout: " + str(self.bufout)
		print(s)

	def __hash__(self):
		bufin = str(self.bufin)
		bufout = str(self.bufout)
		return hash(('False', bufout, bufin, self.state.__hash__()))

	def __eq__(self, other):
		return self.__hash__() == other.__hash__()

class Pgm(Configuration):
	def __init__(self, _vars, stmt, buf):
		self.vars = _vars
		self.stmt = stmt
		self.buf = buf

	def execute(self):
		state = hashabledict()
		for var in self.vars:
			if var not in state:
				state[var] = 0

		return self.stmt.execute(state, self.buf)

# Stmt
class StmtLet(Configuration):
	def __init__(self, _id, aexp, block):
		self.id = _id
		self.aexp = aexp
		self.block = block

	def execute(self, state, buf):
		conf_result = set()
		confs = self.aexp.execute(state, buf)
		for conf in confs:
			if conf.value == StateType.ERROR:
				conf_result.add(ConfHalting(conf.state, conf.buf, StateType.EPSILON))
				continue

			if self.id not in conf.state:
				print(self.id, "not defined!")
				sys.exit()

			new_state = copy.deepcopy(conf.state)
			new_state[self.id] = conf.value
			res_configs = self.block.execute(new_state, conf.buf)
			for res_config in res_configs:
				res_config.state = conf.state
				conf_result.add(res_config)
		return conf_result


class StmtSeq(Configuration):
	def __init__(self, stmt1, stmt2):
		self.stmt1 = stmt1
		self.stmt2 = stmt2

	def real_execute(self, stmt1, stmt2, state, buf):
		states = set()
		states1 = stmt1.execute(state,buf)
		for state1 in states1:
			if type(state1) == ConfHalting:
				states.add(state1)
				continue
			states2 = stmt2.execute(state1.state, state1.bufin)
			for state2 in states2:
				bufout = []
				bufout.extend(buf)
				if state1.bufout != StateType.EPSILON:
					bufout.extend(state1.bufout)
				if state2.bufout != StateType.EPSILON:
					bufout.extend(state2.bufout)
				if len(bufout) == 0:
					bufout = StateType.EPSILON

				if type(state2) == ConfHalting:
					states.add(ConfHalting(state2.state, state2.bufin, bufout))
					continue

				states.add(ConfState(state2.state, state2.bufin, bufout))
		return states

	@debug
	def execute(self, state, buf):
		states = set()
		if type(self.stmt1) == StmtSpawn:
			states = states.union(self.real_execute(self.stmt1, self.stmt2, copy.deepcopy(state), buf))
			states = states.union(self.real_execute(self.stmt2, self.stmt1, copy.deepcopy(state), buf))
		else:
			states = states.union(self.real_execute(self.stmt1, self.stmt2, state, buf))

		return states

	def printf(self):
		print(self.stmt1, self.stmt2)


class StmtIf(Configuration):
	def __init__(self, bexp, stmt1, stmt2):
		self.bexp = bexp
		self.stmt1 = stmt1
		self.stmt2 = stmt2

	@debug
	def execute(self, state, buf):
		res_config = set()
		configs = self.bexp.execute(state, buf)
		for config in configs:
			if config.value == StateType.ERROR:
				res_config = res_config.union(ConfHalting(config.state, config.buf, StateType.EPSILON))
				continue
			if config.value:
				res_config = res_config.union(self.stmt1.execute(copy.deepcopy(config.state), config.buf))
				continue
			if not config.value:
				res_config = res_config.union(self.stmt2.execute(copy.deepcopy(config.state), config.buf))

		return res_config

	def printf(self):
		print(self.bexp, self.stmt1, self.stmt2)

class StmtWhile(Configuration):
	def __init__(self, bexp, stmt):
		self.bexp = bexp
		self.stmt = stmt

	@debug
	def execute(self, state, buf):
		# print("entering while....")
		res_config = set()
		configs = self.bexp.execute(state, buf)
		for config in configs:
			if config.value == StateType.ERROR:
				res_config.add(ConfHalting(config.state, config.buf, StateType.EPSILON))
				continue

			if not config.value:
				# print("should go out")
				res_config.add(ConfState(config.state, config.buf, StateType.EPSILON))
				continue

			stmt = StmtSeq(self.stmt, StmtWhile(self.bexp, self.stmt))
			res_config = res_config.union(stmt.execute(config.state, config.buf))

		# print("exiting while....")
		return res_config

	def printf(self):
		self.bexp.printf()
		self.stmt.printf()
		
class StmtBlock(Configuration):
	def __init__(self, stmt):
		self.stmt = stmt

	def execute(self, state, buf):
		return self.stmt.execute(state, buf)

class StmtAssign(Configuration):
	def __init__(self, _id, exp): 
		self.id = _id
		self.exp = exp

	@debug
	def execute(self, state, buf):
		states = set()
		states1 = self.exp.execute(state, buf)
		for state1 in states1:
			if state1.value != StateType.ERROR:
				if self.id not in state1.state:
					print("At StmtAssign", self.id, "not defined!")
					sys.exit()
				state1.state[self.id] = state1.value
				states = states.union([ConfState(state1.state, state1.buf, StateType.EPSILON)])
			else:
				states = states.union([ConfHalting(state1.state, state1.buf, StateType.EPSILON)])
		return states

	def printf(self):
		print(self.id, self.exp)

class StmtPrint(Configuration):
	def __init__(self, exp):
		self.exp = exp

	def execute(self, state, buf):
		states = set()
		configs = self.exp.execute(state, buf)
		for config in configs:
			if config.value == StateType.ERROR:
				states = states.union([ConfHalting(config.state, config.bufin, StateType.EPSILON)])
				continue
	
			states = states.union([ConfState(config.state, config.buf, [config.value])])
		return states

class StmtSpawn(Configuration):
	def __init__(self, stmt):
		self.stmt = stmt

	def execute(self, state, buf):
		return self.stmt.execute(state, buf)

class StmtHalt(Configuration):
	def execute(self, state, buf):
		return set(ConfHalting(state, buf, StateType.EPSILON))

# BExp
class BExpNot(Configuration):
	def __init__(self, exp):
		self.exp = exp

	@debug
	def execute(self, state, buf):
		res_config = set()
		configs = self.exp.execute(state, buf)
		for config in configs:
			if config.value != StateType.ERROR:
				config.value = not config.value
			res_config.add(config)
		return res_config

	def printf(self):
		print(self.exp)

class BExpOp(Configuration):
	def __init__(self, op, exp1, exp2):
		self.op = op
		self.exp1 = exp1
		self.exp2 = exp2

	# deal with non-deterministic on bexp as well
	def real_execute(self, exp1, exp2, state, buf, reverse):
		confs = set()
		confs1 = exp1.execute(state, buf)
		for conf1 in confs1:
			if conf1.value == StateType.ERROR or (self.op == Op.AND and not conf1.value and not reverse):
				confs = confs.union(conf1)
				continue
			confs2 = exp2.execute(conf1.state, conf1.buf)
			for conf2 in confs2:
				if conf2.value == StateType.ERROR:
					confs = confs.union(conf2)
					continue

				if reverse:
					i1 = conf2.value
					i2 = conf1.value
				else:
					i1 = conf1.value
					i2 = conf2.value

				if self.op == Op.LEQ:
					confs = confs.union([ConfExp(i1 <= i2, conf2.state, conf2.buf)])
				if self.op == Op.AND:
					confs = confs.union([ConfExp(i1 and i2, conf2.state, conf2.buf)])
		return confs

	@debug
	def execute(self, state, buf):
		confs = set()
		confs = confs.union(self.real_execute(self.exp1, self.exp2, copy.deepcopy(state), copy.deepcopy(buf), False))
		confs = confs.union(self.real_execute(self.exp2, self.exp1, copy.deepcopy(state), copy.deepcopy(buf), True))
		return confs

	def printf(self):
		print(self.op, self.exp1, self.exp2)


class BExpConst(Configuration):
	def __init__(self, b):
		self.b = b

	def execute(self, state, buf):
		return set([ConfExp(b, state, buf)])

# AExp
class AExpOp(Configuration):
	def __init__(self, op, aexp1, aexp2):
		self.op = op
		self.aexp1 = aexp1
		self.aexp2 = aexp2

	def real_execute(self, exp1, exp2, state, buf, reverse):
		confs = set()
		confs1 = exp1.execute(state,buf)
		for conf1 in confs1:
			if conf1.value == StateType.ERROR:
				confs.add(conf1)
				continue
			confs2 = exp2.execute(conf1.state,conf1.buf)
			for conf2 in confs2:
				if conf2.value == StateType.ERROR:
					confs.add(conf2)
					continue

				if reverse:
					i1 = conf1.value
					i2 = conf2.value
				else:
					i1 = conf2.value
					i2 = conf1.value

				if self.op == Op.PLUS:
					confs.add(ConfExp(i1+i2, conf2.state, conf2.buf))
				if self.op == Op.DIVIDE:
					if i2 == 0:
						confs.add(ConfExp(StateType.ERROR, conf2.state, conf2.buf))
					else:
						confs.add(ConfExp(i1/i2, conf2.state, conf2.buf))
		return confs

	def execute(self, state, buf):
		confs = set()
		# deal with non-deterministic

		confs = confs.union(self.real_execute(self.aexp1, self.aexp2, copy.deepcopy(state), copy.deepcopy(buf), False))
		confs = confs.union(self.real_execute(self.aexp2, self.aexp1, copy.deepcopy(state), copy.deepcopy(buf), True))
		return confs

class AExpConstant(Configuration):
	def __init__(self, i):
		self.i = i

	def execute(self,state,buf):
		return set([ConfExp(self.i, state, buf)])

class AExpId(Configuration):
	def __init__(self, _id):
		self.id = _id

	@debug
	def execute(self, state, buf):
		if self.id not in state:
			print("At AExpId", self.id, "not defined!")
			sys.exit()

		return set([ConfExp(state[self.id], state, buf)])

	def printf(self):
		print(self.id)

class AExpPP(Configuration):
	def __init__(self, _id):
		self.id = _id

	@debug
	def execute(self, state, buf):
		if self.id not in state:
			print("At AExpPP", self.id, "not defined!")
			sys.exit()

		state[self.id] += 1
		return set([ConfExp(state[self.id], state, buf)])

	def printf(self):
		print(self.id)

class AExpRead(Configuration):
	def execute(self, state, buf):
		return set([ConfExp(buf.pop(0), state, buf)])

