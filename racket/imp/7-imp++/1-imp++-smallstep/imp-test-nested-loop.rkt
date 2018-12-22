#lang reader "imp-reader.rkt"

int i, j, s;
while ( i <= 3 ) {
    j = 0;
    while ( j <= 3 ) {
        s = s + 1;
        j = j + 1;
    }
    i = i + 1;
}
print(s);

