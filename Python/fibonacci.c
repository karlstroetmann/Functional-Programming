#include <stdio.h>

// C: Fibonacci function (naive recursive)
unsigned long fib(unsigned long n) {
    if (n == 0) {
        return 0;
    } else if (n == 1) {
        return 1;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

int main() {
    unsigned long n = 40;  // Calculate the 40th Fibonacci number
    printf("%lu\n", fib(n));
    return 0;
}
