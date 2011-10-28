int gcd(int m, int n)
{
        int tmp;
        while(m > 0 && n > 0) { tmp = m; m = n % m; n = tmp; }       
        return n;
}
