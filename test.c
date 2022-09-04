int arr[50];

int printf(char*, ...);
int getchar();
int toupper(int c);


int main(){
	for (int i = 0; i < 50; ++i)
	{
		arr[i] = getchar();
		if (arr[i] == -1){
			break;
		}
	}
	for (int i = 49; i > -1; --i)
	{
		printf("%c", toupper(arr[i]));
	}
}