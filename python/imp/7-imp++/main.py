from big_step import *

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