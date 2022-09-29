#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <stdarg.h>

struct CallFrame
{
	unsigned line;
	const char *func, *file;
	struct CallFrame *next;
};

static struct CallFrame base = {.line=0, .func="main(argc, argv, env)", .file="Progam startup", .next=NULL};
static int gargc;
static const char **gargv;
#define CALL_LIMIT 1024
static unsigned calll_limits = 0;

struct CallFrame *__libtrace_now = NULL;

static void print_main(){
	fprintf(stderr, "%d, ", gargc);
	for (int i = 0; i < gargc; ++i)
	{
		fprintf(stderr, "\"%s\", ", gargv[i]);
	}
	fprintf(stderr, "%s", "NULL)\e[0m\n");
}

void _Noreturn __libtrace_print_stacktrace(const char *msg){
	fprintf(stderr, "%s", "\e[31mTraceback (most recent call last):\e[0m\n");
	struct CallFrame *last = &base;
	struct CallFrame *p = base.next;
	fprintf(stderr, "%s", "  File \e[36m<command-line>\e[0m, in \e[32mmain(");
	print_main();
	while(p) {
		if (last->line == p->line && last->file == p->file){
			unsigned i = 0;
			do{
				++i;
				last = p;
				p = p->next;
				if (p == NULL)
				{
					break;
				}
			} while (last->line == p->line && last->file == p->file);
			fprintf(stderr, "  \e[35m[Previous line repeated %u more times]\e[0m\n", i);
			if (p == NULL)
			{
				break;
			}
		}
		fprintf(stderr, "  File \e[36m\"%s\"\e[0m, line %u, in \e[32m%s\e[0m\n", p->file, p->line, p->func);
		last = p;
		p = p->next;
	}
	fprintf(stderr, "\e[31m%s\e[0m\n", msg);
	exit(EXIT_FAILURE);
}
static const char *get_sig_str(int sig){
	switch (sig){
		case SIGINT: return "SIGINT: Interupt";
		case SIGABRT: return "SIGABRT: Abnormal termination(maybe abort was called?)";
		case SIGSEGV: return "SIGSEGV: Segmentation fault";
		case SIGFPE: return "SIGFPE: erroneous arithmetic operation";
		default: return "(unknown signal)";
	}
}
static void _Noreturn sig_handler(int sig){
	__libtrace_print_stacktrace(get_sig_str(sig));
}
void _Noreturn __libtrace_no_mem(void){
	__libtrace_print_stacktrace("no enough memory: allocation failed");
}
void _Noreturn __libtrace_raise(const char *fmt, ...){
	va_list ap;
	va_start(ap, fmt);
	char *mem = malloc(1024);
	vsnprintf(mem, 1024, fmt, ap);
	va_end(ap);
	__libtrace_print_stacktrace(mem);
}
void __libtrace_begin_call(){
	if (++calll_limits > CALL_LIMIT){
		__libtrace_raise("stack overflow, maximum call limits exceeded: (current limit = %u)", CALL_LIMIT);
	}
}
void __libtrace_end_call(){
	--calll_limits;
}
void __libtrace_init(int argc, const char* argv[]){
	gargc = argc;
	gargv = argv;
	__libtrace_now = &base;
	signal(SIGINT, sig_handler);
	signal(SIGSEGV, sig_handler);
	signal(SIGABRT, sig_handler);
	signal(SIGFPE, sig_handler);
}

#ifdef TEST
#define CALL(f) {struct CallFrame n = {.line=__LINE__, .func=#f, .file=__FILE__}, *old=__libtrace_now;__libtrace_now->next=&n;__libtrace_now=&n;f;__libtrace_now=old;__libtrace_now->next = NULL;}
int Bar(){
	int *p = NULL;
	return *p;
}
void Foo(){
	CALL(Bar());
}
int main(int argc, const char *argv[], const char *env[]){
	__libtrace_init(argc, argv);
	CALL(Foo());
}
#endif
