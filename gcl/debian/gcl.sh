#!/bin/sh

EXT=@EXT@
VERS=@VERS@

. /etc/default/gcl$EXT
if ! set | grep -q -w GCL_ANSI ; then GCL_ANSI=$DEFAULT_GCL_ANSI ; fi
if ! set | grep -q -w GCL_PROF ; then GCL_PROF=$DEFAULT_GCL_PROF ; fi

DIR=/usr/lib/gcl-$VERS;

if [ "$GCL_ANSI" = "" ] ; then 
    if [ "$GCL_PROF" = "" ] ; then
	EXE=saved_gcl;
    else
	EXE=saved_gcl_gprof;
    fi
else
    if [ "$GCL_PROF" = "" ] ; then
	EXE=saved_ansi_gcl;
    else
	EXE=saved_ansi_gcl_gprof;
    fi
fi
SYS=$DIR/unixport

exec $SYS/$EXE -dir $SYS/ -libdir $DIR/ \
   -eval '(setq si::*allow-gzipped-file* t)' \
   -eval '(setq si::*tk-library* "/usr/lib/tk@TKVERS@")' \
     "$@"

# other options: -load /tmp/foo.o -load jo.lsp -eval "(joe 3)"
