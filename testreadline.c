/*
$ gcc testreadline.c -lreadline -ltinfo
*/

#include <stdlib.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>

int main()
{
    for(;;) {
        char* line = readline("ready>");
        if (line){
            add_history(line);
            fprintf(stderr, "read: %s\n", line);
            free(line);
            continue;
        }
        break;
    }
}