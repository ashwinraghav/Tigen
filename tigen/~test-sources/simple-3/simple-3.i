int simple(int x, int y) {
  int z;
  if (x == y) {
    z = 1;
  } else if (x * y == 25) {
    z = 2; 
  } else if (x / y == 25) {
    z = 3; 
  } 
  if (x + y + z == 133) {
    z = 4; 
  } 
  return z; 
} 
