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
    longPgm = "int x, y, z; x = 2 + 3; if (x <= 5 && !(x <= 4)) { y = 2 } else { y = 1 } ; z = 1 ; while (!(y<=0)) {z = z + z ; y = y + -1}"
    spawnPgm = "int x; spawn { x = 0 } ; x = 1" -- This program has two possible end states, either x = 0 or x = 1 depending on which is run first
    spawn2Pgm = "int w, x, y, z, n; n = 3; while (x <= n && y<= n) { w = x; z = y; { spawn { x = x + 1 ; y = z } ; { y = y + 1 ; x = w } } ; z = y; w = x }" -- has (n+1) * 2 possible states where z = x = [0,n] and y = w = [0,n]

    programs = [empty, skipSeqComp, assignment, assignmentSeqComp, ifPgm, whilePgm, whileNotPgm, whileNotPgm2, divPgm, longPgm, spawnPgm, spawn2Pgm]