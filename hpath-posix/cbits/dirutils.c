#include "dirutils.h"

#include <dirent.h>

#ifdef __APPLE__
#include <stdio.h>
#include <sys/param.h>
#include <sys/fcntl.h>
#include <unistd.h>
#endif

unsigned int
    __posixdir_d_type(struct dirent* d)
    {
      return(d -> d_type);
    }


extern DIR*
	__posixdir_fdopendir(int fd)
{
#ifdef __APPLE__
	char fullpath[MAXPATHLEN];
	DIR *d;

	if(fcntl(fd, F_GETPATH, fullpath) < 0) {
		perror("fcntl");
		fprintf(stderr, "Unable to convert file descriptor back to pathname in __posixdir_fdopendir().\n");
		return NULL;
	}
	if(close(fd) < 0) {
		perror("close(fd) in __posixdir_fdopendir wrapper:");
		return NULL;
	}

	d = opendir(fullpath);
	return d;
#else
	return fdopendir(fd);
#endif
}
