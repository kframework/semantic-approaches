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
    longPgm = ("int x, y, z; x = 2 + 3; if (x <= 5 && !(x <= 4)) { y = 2 } else { y = 1 } ; z = 1 ; while (!(y<=0)) {z = z + z ; y = y + -1}", [])
    divBy0 = "int x; x = 1 / 0; x = 2"
    halt = "int x; halt ; x = 2"
    fromBook = "int m, n, s; n = 100 ; while (true) { if (m <= n) { s = s + m ; m = m + 1 } else { halt } }"
    fromBook2 = "int m, n, s; n = 100 ; while (true) { if (m <= n) { s = s + m ; m = m + 1 } else { s = s / (n / m) } }"

    programs = [empty, skipSeqComp, assignment, assignmentSeqComp, ifPgm, whilePgm, whileNotPgm, whileNotPgm2, divPgm, divBy0, halt, fromBook, fromBook2]