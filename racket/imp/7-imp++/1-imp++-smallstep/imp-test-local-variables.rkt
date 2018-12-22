#lang reader "imp-reader.rkt"

int x, y;
x = 1;
y = 2;
print(x);
print(y);
{ int x;
  x = 3;
  y = 4;
  print(x);
  print(y);
}
int a;
a = 5;
print(x);
print(a);
