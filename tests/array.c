int printf(const char*, ...);

void zero(int *arr, int n){
    for (int i = 0; i < n; ++i)
    {
        arr[i] = 0;
    }
}

void set(int *arr, int n, int v){
    for (int i = 0; i < n; ++i)
    {
        arr[i] = v;
    }
}

void print(int *arr, int n){
    for (int i = 0; i < n; ++i)
    {
        printf("%d\n", arr[i]);
    }
}

void get(int *arr, int n){
    for (int i = 0; i < n; ++i)
    {
        arr[i] = i;
    }
}

int main(){
    int arr[10];
    zero(arr, 10);
    print(arr, 10);
    set(arr, 10, 6);
    print(arr, 10);
    get(arr, 10);
    print(arr, 10);
}
