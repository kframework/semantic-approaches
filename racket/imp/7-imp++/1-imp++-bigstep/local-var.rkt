#lang reader "imp-reader.rkt"

int x, y;
x = 1;
y = 2;

{ int x;
  x = 3;
  y = 4;
  print(x);
  print(y);
}

print(x);
print(y);
