#include <stdio.h>

int derp(int arr[3]);
int derping();

int test(int a, int b) {
    return a && b;
}

int fuck(int a[2]) {
  return a[0] + a[1];
}

int main() {
    int derper[3] = {1,222, 42};
    printf("Output derp: %d\n", derp(derper));
    printf("Output fuck: %d\n", fuck(derper));
    printf("Output derping: %d\n", derping());
    return 1;
}

int t(int *x, int xlen) {
  for(int i = 0; i < xlen; i++) {
    x[i] = 0;
  }
  return 1;
}