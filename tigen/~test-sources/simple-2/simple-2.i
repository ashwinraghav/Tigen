typedef struct s{
  int a;
};
int simple(struct s foo) {
  if (foo.a > 10) {
    return 0;
  } 
  return 1; 
} 
