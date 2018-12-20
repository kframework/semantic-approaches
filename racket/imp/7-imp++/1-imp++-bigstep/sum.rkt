#lang reader "imp-reader.rkt"

int n, sum;
while (n <= 9) {
  sum = sum + ++n;
}
print(sum);