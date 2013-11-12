#include <stdio.h>
#include <string.h>
#include "efficienC.h"

#define MEMSIZE 100

int memory[MEMSIZE];

WPTR_MOD int *wptr WPTR_REG;
FPTR_MOD int *fptr FPTR_REG;
BPTR_MOD int *bptr BPTR_REG;
int *tptr;
int tnext;

int main(int argc, char *argv[])
{
int i;

wptr = &(memory[MEMSIZE - 1]);
for(i = 0; i < MEMSIZE; i++)
  memory[i] = 0xdeadbeef;

fptr = NOT_PROCESS_P;
bptr = NOT_PROCESS_P;

occam_program();

return 0;
}
