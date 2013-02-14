#include<stdio.h>

void oneUp(int *vec, int *vec_len){
 int i=0;
 if ((*vec_len)>1){
  for(i=0; i<(*vec_len)-1;i++){
	vec[i]=vec[i+1];
	vec[(*vec_len)-1]=0;
  }
 }
}
