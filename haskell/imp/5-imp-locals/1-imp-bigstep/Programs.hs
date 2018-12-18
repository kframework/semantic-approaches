module Programs (programs) where
    
    empty = "int ; skip"
    skipSeqComp = "int x; skip ; skip"
    assignment = "int x; x = 2"
    assignmentSeqComp = "int x, y; x = 2 + 4 ; y = 2 + 3"
    ifPgm = "int x; if ( x <= 0 ) { x = 1 } else { }"
    whilePgm = "int x, y; while (x <= 2) { skip ; y = 2 ; x = x + 1 }"
    whileNotPgm = "int x; while (!(x <= 2)) { skip ; y = 2 }"
    whileNotPgm2 = "int x, y; x = 3; while (!(x <= 2)) { skip ; y = y + -1 ; x = x + -1 }"
    divPgm = "int x, y; x = 4; y = 2; x = x / y"
    shadow1 = "int x, y; x = 2 ; y = 2; { int x; int y; x = 1 ; x = x + 1 ; { x = x + 2 ; y = 1} ; y = 3}" -- This should end with x = 2, y = 2
    shadow2 = "int x, y; x = 2 ; y = 1; { int x; x = 1 ; x = x + 1 ; { x = x + 2 ; y = 2} ; y = y + 1}" -- This should end with x = 2, y = 3
    locals = "int x, y; { int w, z; w = 5; z = 6; { x = w ; { int q; q = z; { int q; q= 0 } ; y = q } } }" -- This should end with x = 5, y = 6

    programs = [empty, skipSeqComp, assignment, assignmentSeqComp, ifPgm, whilePgm, whileNotPgm, whileNotPgm2, divPgm, shadow1, shadow2, locals]