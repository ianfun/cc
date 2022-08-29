struct test
{
int lion;
char msg;   
} globalvar;

int main(int argc, char const *argv[])
{
    globalvar.msg = "world";
    return 0;
}