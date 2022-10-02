int getchar(void);

int (*f(void))(void){
	return getchar;
}

int test(){
	return f()();
}
