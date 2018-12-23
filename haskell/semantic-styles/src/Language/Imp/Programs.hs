module Language.Imp.Programs
where

import Data.Text as T

program1 = T.pack "int x,y,z; while (x<=10) {x = x+1;}"
sumPgm = T.pack "int n, s ;          \
                \n = 100 ;           \
                \while (!(n <= 0)) { \
                \  s = s + n ;       \
                \  n = n + -1 ;      \
                \} "


collatzStmt = T.pack "while (!(n <= 1)) {     \
                     \  s = s + 1 ;           \
                     \  q = n / 2 ;           \
                     \  r = q + q + 1 ;       \
                     \  if (r <= n) {         \
                     \    n = n + n + n + 1 ; \
                     \  } else {              \
                     \    n = q ;             \
                     \  }                     \
                     \} "

collatzPgm = T.append
             ( T.pack
               "int m, n, q, r, s ; \
               \m = 10 ; \
               \while (!(m <= 2)) { \
               \  n = m ; \
               \  m = m + -1 ; "
             )
             $ T.snoc collatzStmt '}'

