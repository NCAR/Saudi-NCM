
/* Strip blank (' '), null ('\0'), feed ('\r') and return ('\n') characters
   at the end of a string,

   FORTRAN usage: 
      INTEGER :: string_len, il 
      CHARACTER (len=string_len) :: string
      CALL return_null(string_len,il,string)
      WRITE (*,'(A)') string (1:il)
*/

#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/uio.h>

#define clean_string   clean_string_

clean_string(name)

    char         name[40];
{
    int           ie;
    int           i;
    int           il;

    /* strip trailing blanks and add null character to name */

    ie=40;
    il = ie;
    for (i = 0; name[i] != '\0' && name[i] != '\n' && '\r' && i < ie; ++i){
/*  printf("i = %d name = %c\n",i,name[i]); */
    }
    il = i;
    for (i = il; i < ie; ++i){
    name[i] = ' ';
    }
/*  printf("il = %d name = %c\n",i); */

    return (0);
}
