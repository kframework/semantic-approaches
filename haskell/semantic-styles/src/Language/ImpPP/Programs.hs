module Language.ImpPP.Programs
where

import Data.Text as T

sumUpPgm = T.pack -- This is way too non-deterministic to execute completely currently
  " int n, m ; n = read() ; m = n / 2 ; \
  \ spawn {                             \
  \ int c, s ;                          \
  \ while( c <= m ) {                   \
  \   s = ++c ;                         \
  \ }                                   \
  \   m = m + s ;                       \
  \ }                                   \
  \ spawn {                             \
  \ int c, s ; c = m + 1 ;              \
  \ while( c <= n ) {                   \
  \   s = ++c                           \
  \ }                                   \
  \ m = m + s ;                         \
  \ }                                   \
  \ print ( m ) ;                       "

incUpPgm = T.pack -- This is a reasonable non-deterministic program
  " int n, c, s ;       \
  \ n = read() ;        \
  \ n = n / 2 ;         \
  \ spawn { s = ++c ; } \
  \ c = n + 1 ;         \
  \ s = ++c ;           \
  \ n = n + s ;         \
  \ print ( n ) ;       "

haltPgm = T.pack
  " int n ;                   \
  \ spawn { n = ++n / ++n ; } \
  \ print ( n ) ;             \
  \ halt ;                    \
  \ print(42);                "
