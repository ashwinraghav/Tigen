int string_test(char * str) {
  int x; 
  if (str) { x = 1; } 
  if (str[1] == '\0') { x = 2; } 
  if (strlen(str) == 3) { x = 3; } 
  if (strcmp(str, "ant") == 0) { x = 4; } 
  return x; 
} 
