#include <stdio.h>

void duck_print_array(void* addr){
  float *ptr=addr;
  int i=0;
  for(i=0;i<8;i++){
    printf("######=%f\n",*ptr);
    ptr++;
  }
}

void* duck_malloc(size_t n){
  void* ptr=malloc(n);
  return ptr; 
}

void duck_free(void* ptr){
  if(ptr!=NULL){
    free(ptr);
  }
}

void duck_print_value(void* ptr){

}

