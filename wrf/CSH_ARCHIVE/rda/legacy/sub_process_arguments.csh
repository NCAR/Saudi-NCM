#!/bin/tcsh -f

set show_help = 0

#set argv=`getopt f:j:p:h $*`
#set argsstr="$argv"

if ( $?argv ) then
  foreach name ($argv)
    switch ($name)
      case -f:
        set env_var_file = $2
        if ( ! -e ${env_var_file} ) then
          echo "  $0 ***** ERROR  [${env_var_file}] does not exist *****"
        endif
        breaksw
      case -j:
        set job_dir = $2
        if ( ! -d ${job_dir} ) then
          echo "  $0 *** INFO  Ignored job_dir [${job_dir}] because it does not exist ***"
        endif
        breaksw
      case -l:
        set log_exec_time = 1
        breaksw
      case -m:
        set do_adjoint_moist = 0
        breaksw
      case -p:
        set perl_dir = $2
        if ( ! -d ${perl_dir} ) then
          echo "  $0 *** INFO  Ignored perl_dir [${perl_dir}] because it does not exist ***"
        endif
        breaksw
      case -r:
        set RANGE = $2
        #echo " RANGE: $RANGE, $1, $2"
        breaksw
      case -t:
        set arg_time = $2
        breaksw
      case -h:
        set show_help = 1
        breaksw
      case --:
        if ( ${#argv} > 1 ) then
          set extra_arg = $2
          #echo " BBB begin_time: $begin_time"
        endif
        breaksw
    endsw

    shift
  end
endif
