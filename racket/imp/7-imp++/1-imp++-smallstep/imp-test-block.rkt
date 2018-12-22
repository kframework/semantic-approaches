#lang reader "imp-reader.rkt"

int x, y;
{
    x = 2;
}

if (! x <= 1) {
    {
        y = x;
    }
} else {
    y = 1;
}



