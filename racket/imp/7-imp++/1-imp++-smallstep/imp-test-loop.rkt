#lang reader "imp-reader.rkt"

int x, s;
while ( x <= 3 ) {
  s = s + x;
  x = x + 1;
}

print(s);
