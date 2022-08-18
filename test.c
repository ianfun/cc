#define x() ( 4 + y() )
#define y() ( 2 * x() )
#define STR(a) #a
#define STR2(a) STR(a)
#define d() 1 + 22222         * 8+9
int main(){
	puts(STR2( x() ));
	puts(STR2( d() ));
}
