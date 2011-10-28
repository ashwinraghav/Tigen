int fib(int n) {
    if ( n == 0 || n == 1 ) 
        return n;
 
    int fib1 = 0; 
    int fib2 = 1;
    int fib = 0;
 
    for ( int i = 2; i < n; i++ ) 
    {
        fib = fib1 + fib2;
        fib1 = fib2;
        fib2 = fib;
    }
 
    return fib;
}
