module Main where
    import Parser
    import Imp_Bigstep
    import Programs

    outParseString p = putStrLn (show (Parser.parseString p)) -- Use to just parse the program string p and print it out
    outRunString p = putStrLn (show (Imp_Bigstep.execute (ConfPgm (Parser.parseString p)))) -- Use to parse then run the program string p
    outParseFile p = Parser.parseFile p >>= \l -> putStrLn (show l) --unused, use "outParseFile <filename>" to parse a program from a file and then print out the result
    outrunFile p = Parser.parseFile p >>= \l -> putStrLn (show (Imp_Bigstep.execute (ConfPgm l))) --unused, use "outrunFile <filename>" to parse and run a program from a file then print out the result

    main = mapM outRunString Programs.programs