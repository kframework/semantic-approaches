module Programs (programs) where
    -- The format here is (program, input buffer)
    empty = ("int ; skip", [])
    skipSeqComp = ("int x; skip ; skip", [])
    assignment = ("int x; x = 2", [])
    assignmentSeqComp = ("int x, y; x = 2 + 4 ; y = 2 + 3", [])
    ifPgm = ("int x; if ( x <= 0 ) { x = 1 } else { }", [])
    whilePgm = ("int x, y; while (x <= 2) { skip ; y = 2 ; x = x + 1 }", [])
    whileNotPgm = ("int x; while (!(x <= 2)) { skip ; y = 2 }", [])
    whileNotPgm2 = ("int x, y; x = 3; while (!(x <= 2)) { skip ; y = y + -1 ; x = x + -1 }", [])
    divPgm = ("int x, y; x = 4; y = 2; x = x / y", [])
    longPgm = ("int x, y, z; x = 2 + 3; if (x <= 5 && !(x <= 4)) { y = 2 } else { y = 1 } ; z = 1 ; while (!(y<=0)) {z = z + z ; y = y + -1}", [])
    readPgm = ("int x, y, z; x = read(); y = read(); z = x + y", [2, 3])
    printPgm = ("int x; print(45) ; print(x + 2); print(x + 7 + -1)", [])
    printPgm2 = ("int x; x = 0; while (x<= 5) { print(x) ; x = x + 1 }", [])
    readPrint = ("int x; print(read()) ; print (read() / 2)", [5, 6])
    readPrint2 = ("int x; print(read() / read()) ", [1 , 2]) -- This program has two possible outputs, either 0 or 2 depending on which read() is evaluated first.

    programs = [empty, skipSeqComp, assignment, assignmentSeqComp, ifPgm, whilePgm, whileNotPgm, whileNotPgm2, divPgm, longPgm, readPgm, printPgm, printPgm2, readPrint, readPrint2]