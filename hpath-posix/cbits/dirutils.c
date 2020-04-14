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
	return fdopendir(fd);
}
