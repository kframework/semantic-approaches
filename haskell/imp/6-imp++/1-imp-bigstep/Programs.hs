module Programs (programs) where
    
    empty = ("int ; ", [])
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
    plusPlus = ("int x, y, z; y = ++x; if (y <= 0) {z = 1} else {}", [])
    plusPlus2 = ("int x, y; x = 7; y = ++x / 2", [])
    plusPlus3 = ("int x, y; x = 7; y = ++x / (x / 4)", []) --This program has two possible results:
                                                         --1: x = 8, y = 4: ++x is evaluated first so it becomes y = 8 / (8 /  4) = 8 / 2 = 4
                                                         --2: x = 8, y = 8: (x / 4) is evaluated first so it becomes y = 8 / (7 / 4) = 8 / 1 = 8
    plusPlus4 = ("int x, y, z; y = 5; while (++x <= y) { z = z + -1 }", [])
    halt = ("int x; halt ; x = 2", [])
    fromBook = ("int m, n, s; n = 100 ; while (true) { if (m <= n) { s = s + m ; m = m + 1 } else { halt } }", [])
    fromBook2 = ("int m, n, s; n = 100 ; while (true) { if (m <= n) { s = s + m ; m = m + 1 } else { s = s / (n / m) } }", [])
    spawnPgm = ("int x, y, z; spawn { x = 0; y = read() } ; x = 1 ; z = read()", [1, 2]) -- This program has two possible end states, either x = 0, y = 1, z = 2 or x = 1, z = 1, y = 2 depending on which is run first
    spawn2Pgm = ("int w, x, y, z, n; n = 3; while (x <= n && y<= n) { w = x; z = y; { spawn { x = x + 1 ; y = z } ; { y = y + 1 ; x = w } } ; z = y; w = x }", []) -- has (n+1) * 2 possible states where z = x = [0,n] and y = w = [0,n]
    shadow1 = ("int x, y; x = 2 ; y = 2; { int x; int y; x = 1 ; x = x + 1 ; { x = x + 2 ; y = 1} ; y = 3}",[]) -- This should end with x = 2, y = 2
    shadow2 = ("int x, y; x = 2 ; y = 1; { int x; x = 1 ; x = x + 1 ; { x = x + 2 ; y = 2} ; y = y + 1}",[]) -- This should end with x = 2, y = 3
    locals = ("int x, y; { int w, z; w = 5; z = 6; { x = w ; { int q; q = z; { int q; q= 0 } ; y = q } } }", []) -- This should end with x = 5, y = 6
    printError = ("int x; print(1/0) ; x = 1", [])
    spawnError = ("int x; spawn { x = 1 / 0 ; x = 2 } ; x = 1", []) -- Ends with either x = 0 or x = 1

    programs = [empty, skipSeqComp, assignment, assignmentSeqComp, ifPgm, whilePgm, whileNotPgm, whileNotPgm2, divPgm, longPgm, readPgm, printPgm, printPgm2, readPrint, readPrint2, plusPlus, plusPlus2, plusPlus3, plusPlus4, halt, fromBook, fromBook2, spawnPgm, spawn2Pgm, shadow1, shadow2, locals, printError, spawnError]