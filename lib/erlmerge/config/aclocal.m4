dnl ----------------------------------------------------------------------
dnl Check for some important directories.
dnl ----------------------------------------------------------------------

AC_DEFUN(BTT_ERL_DIR,
[

    case "$target_os" in
	*cygwin*)
	  ERLDIR=`echo ${ERL} | sed 's/\/erts.*//'`
          ERL_DLL_LIB=`ntpath.sh ${ERLDIR}/usr/lib/erl_dll.lib`
          AC_SUBST(ERL_DLL_LIB)
	;;
	*)	
    	  ERLDIR=`awk -F= '/ROOTDIR=/ { print [$]2; exit; }' $ERL`;;
    esac

    if test ! -d $ERLDIR ; then
	AC_MSG_ERROR([Broken Erlang installation, $ERLDIR does not exist!])
    fi
])dnl

AC_DEFUN(BTT_YAWS_DIR,
[

    YAWSDIR=`awk -F= '/yawsdir=/ { print [$]2; exit; }' $YAWS | sed s/\"//g`

    if test ! -d $YAWSDIR ; then
	AC_MSG_ERROR([Broken Yaws installation, $YAWSDIR does not exist!])
    fi
])dnl


dnl ----------------------------------------------------------------------
dnl BTT_REQUIRE_PATH_PROG, like AC_PATH_PROG but fails if 
dnl the program is not found.
dnl ----------------------------------------------------------------------

AC_DEFUN(BTT_REQUIRE_PATH_PROG,
[
  AC_PATH_PROG($1, $2, no, $3)
  case [$]$1 in
      /*)
	# Ok
	;;
      no)
        # Not found
        AC_MSG_ERROR("$1 not found in path!")
        ;;
      *)
        # Not an absoluet path
        AC_MSG_ERROR("Could not find absolute path to $1")
        ;;
  esac
])dnl

