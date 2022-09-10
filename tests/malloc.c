typedef unsigned  long long size_t;
void *malloc(size_t size);
void free(void*);

int main(){
	int *p = malloc(100);
	p[0] = 10;
	free(p);
}
