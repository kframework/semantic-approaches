from big_step import *

# int x, y;
# x = y+1
# y = x
pgm_basic = Pgm(['x','y'], 
		  StmtSeq(StmtAssign('x', AExpOp(Op.PLUS, AExpConstant(1), AExpId('y'))),
		  	   StmtAssign('y', AExpId('x')) 
		  	   ), 
		  [])
results = pgm_basic.execute()
print("The evaluation result for pgm_basic: ")
for result in results:
	result.printf()

print("*&^%#$%^&**&^%$")

# int x;
# while(!(2 <= x))
#	x = (++x)
pgm_while = Pgm(['x', 'p', 'q'],
		   StmtWhile(BExpNot(BExpOp(Op.LEQ, AExpConstant(4), AExpId('x'))),
				   	 StmtSeq(StmtAssign('x', AExpPP('x')),
					  		 StmtIf(BExpOp(Op.LEQ, AExpConstant(2), AExpId('x')),
					  		  	    StmtAssign('q', AExpOp(Op.PLUS, AExpConstant(1), AExpId('q'))),
					  		  	    StmtAssign('p', AExpOp(Op.PLUS, AExpConstant(1), AExpId('p')))
					  		  	    )
					  		)
		   	  		),
		   []
		)

results = pgm_while.execute()
print("The evaluation result for pgm_while: ")
for result in results:
	result.printf()
print("*&^%#$%^&**&^%$")

# int x, y, s, q, p;
# x = -3
# while(!(10 <= y)):
#   y = y + 1
# 	s = x/(++x) + s
#	if s <= y:
#		q = q +1
# 	else:
#		p = p + 1
pgm_pp_while_if_halt = Pgm(['x', 'y', 's', 'q', 'p'],
			StmtSeq(StmtAssign('x', AExpConstant(-3)),
					StmtWhile(BExpNot(BExpOp(Op.LEQ, AExpConstant(10), AExpId('y'))), 
					  		  StmtSeq(StmtAssign('y', AExpOp(Op.PLUS, AExpConstant(1), AExpId('y'))),
							  		  StmtSeq(StmtAssign('s', AExpOp(Op.PLUS, 
							  		  								 AExpOp(Op.DIVIDE, AExpId('x'), AExpPP('x')),
							  		  								 AExpId('s'))),
							  		  		  StmtIf(BExpOp(Op.LEQ, AExpId('s'), AExpId('y')),
							  		  		  		 StmtAssign('q', AExpOp(Op.PLUS, AExpConstant(1), AExpId('q'))),
							  		  		  		 StmtAssign('p', AExpOp(Op.PLUS, AExpConstant(1), AExpId('p')))
							  		  		  		 )
							  		  		  )
					  		  		 )
					  )
			),
			[])

results = pgm_pp_while_if_halt.execute()
print("The evaluation result for pgm_pp_while_if_halt: ")
for result in results:
	result.printf()
print("*&^%#$%^&**&^%$")

# int x, y;
# y = 1
# let y = (x++) in 
#	  y = y + 1
pgm_let = Pgm(['x', 'y'],
			  StmtSeq(StmtAssign('y', AExpConstant(1)),
			  		  StmtLet('y', 
			  		  	      AExpPP('x'), 
			  		  		  StmtBlock(StmtAssign('y', AExpOp(Op.PLUS, AExpConstant(1), AExpId('y')))))
			  ),
              [])
results = pgm_let.execute()
print("The evaluation result for pgm_let: ")
for result in results:
	result.printf()
print("*&^%#$%^&**&^%$")

# int x ;
# {
#     spawn {x = x + 1 ;}
#     spawn {x = x + 10 ;}
# } --- such grouping can appear, e.g., when unrolling loops
# x = x + 100 ;     ) .
pgm_spawn = Pgm(['x'], 
				StmtSeq(StmtBlock(StmtSeq(StmtSpawn(StmtAssign('x', AExpOp(Op.PLUS, AExpConstant(1), AExpId('x')))),
				 		 		   		  StmtSpawn(StmtAssign('x', AExpOp(Op.PLUS, AExpConstant(10), AExpId('x'))))
				 		 		   		  )
							     ), 
				 		StmtAssign('x', AExpOp(Op.PLUS, AExpConstant(100), AExpId('x')))
				        ),
				 [])
results = pgm_spawn.execute()
print("The evaluation result for pgm_spawn: ")
for result in results:
	result.printf()
print("*&^%#$%^&**&^%$")



# sumIOPgm = (
#    int m, n, s ;
#    n = read() ;
#    while (m <= n) {
#      print(m) ;
#      s = s + m ;
#      m = m + 1 ;
#    }
#    print(s) ;    ) .
pgm_io = Pgm(['m', 'n', 's'],
			 StmtSeq(StmtAssign('n', AExpRead()),
			 	     StmtSeq(StmtWhile(BExpOp(Op.LEQ, AExpId('m'), AExpId('n')), 
							  		   StmtSeq(StmtPrint(AExpId('m')), 
							  		  		   StmtSeq(StmtAssign('s', AExpOp(Op.PLUS,  AExpId('m'), AExpId('s'))),
							  		  		   		   StmtAssign('m', AExpOp(Op.PLUS, AExpConstant(1), AExpId('m')))
							  		  		   		   )
					  		  				   )
					  				   ),
			 	     		StmtPrint(AExpId('s'))
			 	     		)
			 		),
			 [10])
results = pgm_io.execute()
print("The evaluation result for pgm_io: ")
for result in results:
	result.printf()
print("*&^%#$%^&**&^%$")

