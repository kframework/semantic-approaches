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
    plusPlus = "int x, y, z; y = ++x; if (y <= 0) {z = 1} else {}"
    plusPlus2 = "int x, y; x = 7; y = ++x / 2"
    plusPlus3 = "int x, y; x = 7; y = ++x / (x / 4)" --This program has two possible results:
                                                     --1: x = 8, y = 4: ++x is evaluated first so it becomes y = 8 / (8 /  4) = 8 / 2 = 4
                                                     --2: x = 8, y = 8: (x / 4) is evaluated first so it becomes y = 8 / (7 / 4) = 8 / 1 = 8
    plusPlus4 = "int x, y, z; y = 5; while (++x <= y) { z = z + -1 }"

    programs = [empty, skipSeqComp, assignment, assignmentSeqComp, ifPgm, whilePgm, whileNotPgm, whileNotPgm2, divPgm, longPgm,plusPlus, plusPlus2, plusPlus3, plusPlus4]