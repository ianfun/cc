int printf(char*, ...);


int main(int argc, char** argv){
	char *src = argv[0];
	int hash = 0;
    while ((*src >= 'a' && *src <= 'z') || (*src >= 'A' && *src <= 'Z') || (*src >= '0' && *src <= '9') || (*src == '_') || (*src == '.')) {
        hash = hash * 147 + *src;
        src++;
    }
    return hash;
}