// http://rosettacode.org/wiki/Equilibrium_index#C
int eq_idx(int *a, int len, int **ret)
{
        int i, sum, s, cnt;
        /* alloc long enough: if we can afford the original list,
         * we should be able to afford to this.  Beats a potential
         * million realloc() calls.  Even if memory is a real concern,
         * there's no garantee the result is shorter than the input anyway */
        *ret = malloc(sizeof(int) * len);
 
        for (i = 0, sum = 0; i < len; i++) sum += a[i];
 
        for (i = cnt = s = 0; i < len; i++) {
                if (s * 2 + a[i] == sum)
                        (*ret)[cnt++] = i;
                s += a[i];
        }
 
        *ret = realloc(*ret, cnt * sizeof(int));
        return cnt;
}
