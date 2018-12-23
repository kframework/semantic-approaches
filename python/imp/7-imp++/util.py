

def debug(func):
	def wrapper_inner(*arg, **kwargs):
		#print("************ Debug ************")
		#if type(func) == StmtWhile
		#print(str(type(func)), arg)
		res = func(*arg, **kwargs)
		#print("&&& result &&&")
		#for re in res:
		#	re.printf()
		#print("*******************************")
		return res

	return wrapper_inner

class hashabledict(dict):
    def __hash__(self):
        return hash(tuple(sorted(self.items())))