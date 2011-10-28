int
dot_product(int *a, int *b, int n)
{
        int sum = 0;
        int i;
 
        for (i = 0; i < n; i++) {
                sum += a[i] * b[i];
        }
 
        return sum;
}
