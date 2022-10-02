int call(int (*fn)(void)){
	return fn();
}
int sum(int a, int b){
	return a + b;
}
int subtract(int a, int b){
	return a - b;
}
int mul(int a, int b){
	return a * b;
}
int div(int a, int b){
	return a / b;
}

int printf(char*, ...);

int getchar(void);

int test(int a, int b, int c)
{
	int (*p[4]) (int x, int y);
  p[0] = sum;
  p[1] = subtract;
  p[2] = mul; 
  p[3] = div;
  return p[c](a, b);
}

int main(){
	printf("%d\n", test(getchar(), getchar(), 1));
}
