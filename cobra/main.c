#include <stdio.h>
#include <stdlib.h>
#include <string.h>


const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
// other error codes here


extern int our_code_starts_here() asm("our_code_starts_here");
extern int print(int val) asm("print");
extern void error(int errCode, int val) asm("error");

int decode_bool(int val) {
  int bits = sizeof(int) * 8;
  int mask_val = 1 << (bits - 1);
  int msb = mask(val, mask_val);
  if (msb) {
     printf("true\n");
  }else {
    printf("false\n");
  }
  return val;
}

int decode_int(int val) {
  int res = val >> 1;
  printf("%d\n", res);
  return res;
}

int mask(int val, int mask_val) {
  return val & mask_val;
}

int decode(int val) {
  int lsb = mask(val, 1);
  if (lsb) {
    return decode_bool(val);
  }else {
    return decode_int(val);
  }
}

int print(int val) {
  return decode(val);
}

void error(int errCode, int val) {
  if (errCode == ERR_NOT_NUMBER) {
    fprintf(stderr, "Expected number, but got %010x\n", val);
  } else if (errCode == ERR_NOT_BOOLEAN) {
    fprintf(stderr, "Expected boolean, but got %010x\n", val);
  } else {
    fprintf(stderr, "Unknown error");
    exit(1);
  }
  exit(errCode);
}

int main(int argc, char** argv) {
  int result = our_code_starts_here();
  print(result);
  return 0;
}
