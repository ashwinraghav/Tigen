typedef struct s{
  int a;
};
int simple(int b) {
  struct s x;
  x.a = b;
  if (x.a > 10) {
    return 0;
  } 
  return 1; 
} 
