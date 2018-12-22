import random
from enum import Enum
import sys

class State(Enum):
	ERROR = 1
	HALTING = 2
	EPSILON = 3


class Configuration():

class 


class Tree(object):
    def __init__(self, left, right, data, data_type):
        self.left = left
        self.right = right
        self.data = data
        self.type = data_type


class BigStep:
	def __init__(self):
		self.log = None
		self.f = open("error_log", "w")
		self.match = {""}

	def __del__(self):
		self.f.close()

	def add(exp, state, w):
		a1 = exp.left
		a2 = exp.right
		i = random.randint(0,1)
		if i == 1:
			r1, state1, w1 = evaluate(a1,state,w)
		else:
			r1, state1, w1 = evaluate(a2,state,w)
		if r1 == State.ERROR:
			return State.ERROR, state1, w1
		else:
			if i == 1:
				r2, state2, w2 = evaluate(a2, state1, w1)
			else:
				r2, state2, w2 = evaluate(a1, state1, w1)
			if r2 == State.ERROR:
				return State.ERROR, state2, w2
			else: 
				return r1+r2, state2, w2

	def int_func(i, state):
		return i

	def lookup(x, state):
		if x in state:
			return state[x]

	def pgm(ids, s):

	def assign(exp, state, w):
		a = exp.right
		r, state1, w1 = evaluate_exp(a, state, w)
		if r == State.ERROR:
			return State.HALTING, state, w, State.EPSILON
		else:
			if exp.left.data not in state1:
				self.f.write("Exit..."+str(exp.left.data)+" has not been declared")
				sys.exit()

			state1[exp.left.data] = r
			return state1, w1, State.EPSILON

	def evaluate_exp(exp, state, w):
		func = match(exp)
		return func(exp, state, w)

	def match(s):
		if s.type == "int":
			return self.int_func
		if s.data == "if":
			return if_func
		if s.data == "=":
			return self.assign
		if s.data == "+":
			return self.add
