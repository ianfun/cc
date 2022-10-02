int getchar(void);

int (*f(void))(void){
	return getchar;
}

int main(){
	return f()();
}

int (*(*foo[10])(void))(void);
