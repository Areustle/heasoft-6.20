#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#define until(a)	while(!(a))
#define unless(a)	if(!(a))
#define null(p)		((p)==NULL)

char *read_alloc_file(FILE *fp);

char*
expand_file_name(char *fn)
{
	char *p;
	FILE *fp;
	int pid, fds[2];
	if ( getuid() != geteuid() || getgid() != getegid() ) {
/* '/bin/csh' complains "Permission denied" in this case */
	quit:
		p = malloc(strlen(fn));
		if ( null(p) ) return NULL;
		return strcpy(p, fn);
	}
	if ( pipe(fds) < 0 ) goto quit;
	p = malloc(9+strlen(fn));
	if ( null(p) ) goto quit;
	sprintf(p, "echo -n %s", fn);
	pid = fork();
	if ( pid < 0 ) {
		close(fds[0]);
		close(fds[1]);
		goto quit;
	}
	if ( 0 == pid ) {	/* child */
		close(fds[0]);	/* close read side */
		dup2(fds[1], 1);
		close(fds[1]);
#if 0	/* test */
		fprintf(stdouterr, "expandfn: %s\n", p);
		fprintf(stdout, "uid:%d, euid:%d, gid:%d, egid:%d\n",
				getuid(), geteuid(), getgid(), getegid());
#endif
		execl("/bin/csh", "/bin/csh", "-fc", p, NULL);
		perror("execl");
		_exit(1);
	}					/* parent */
	free(p);
	close(fds[1]);		/* close write side */
	fp = fdopen(fds[0], "r");
	p = read_alloc_file(fp);
	pclose(fp);
	wait(NULL);
	if ( null(p) ) goto quit;
	return p;
}

#if 0
int
main(int argc, char **argv)
{
	int i;
	char *p;
	for (i = 1; i < argc; i++) {
		p = expand_file_name(argv[i]);
		printf("%s --> %s\n", argv[i], p);
	}
	return 0;
}
#endif
