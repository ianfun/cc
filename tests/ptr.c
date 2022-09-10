int a;
char* src;

int next(){
	return a++;
}

int next2(){
	return ++a;
}

char* next3(){
	return ++src;
}

char* next4(){
	return src ++;
}
long long diff(char* a, char *b){
	return a - b;
}
int* add(int *base){
	return base + 10;
}
long long diffint(int* a, int* b){
	return a - b;
}
void test(int a){
	*++src = a;
}