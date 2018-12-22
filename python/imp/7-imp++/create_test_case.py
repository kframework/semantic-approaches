
from big_step import *
# def __init__(self, left, right, data, data_type):

# class DataType(Enum):
# 	PGM = 1
# 	INT = 2
# 	AEXP = 3
# 	BEXP = 4
# 	STMT = 5

# # int x, y;
# # x = y+1;
# # y = x;
# test_case1 = Tree(Tree(Tree(None, None, ['x','y'], DataType. ), None, "declare", DataType.STMT), 
# 				  Tree(Tree(), Tree(), ";", DataType.STMT), 
# 				  None, DataType.PGM)


# int x, y;
# x = y+1
# y = x
pgm = Pgm(['x','y'], 
		  StmtSeq(StmtAssign('x', AExpOp(Op.PLUS, AExpConstant(1), AExpId('y'))),
		  	   StmtAssign('y', AExpId('x')) 
		  	   ), 
		  [])
result = pgm.execute()
result.printf()
