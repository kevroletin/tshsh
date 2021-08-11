#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/ioctl.h>

/* Acquire a controlling terminal (assuming stdin to be a tty)
 * and execute a given program with given arguments.
 */
int main(int argc, char *argv[], char *envp[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s command [arg1 arg2 ...]:\n", argv[0]);
        exit(-1);
    }

    // start a new session
    if (setsid() == -1) {
        fprintf(stderr, "Failed to start a new session\n");
        exit(-1);
    }

    // acquire stdin as a controlling terminal
    if (ioctl(0, TIOCSCTTY, 0) == -1) {
        fprintf(stderr, "Failed to acquire a terminal\n");
        exit(-1);
    }

    execvp(argv[1], (argv + 1));

    return 0;
}
