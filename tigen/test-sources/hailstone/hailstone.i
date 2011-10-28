// http://rosettacode.org/wiki/Hailstone_sequence#C
int hailstone(int i) {
  while (i == i) {
    if (i <= 1) 
      return 1; 
    if ((i % 2) == 0) 
      i = i / 2 ;
    else
      i = (i * 3) + 1;
  } 
  return 2; 
} 
