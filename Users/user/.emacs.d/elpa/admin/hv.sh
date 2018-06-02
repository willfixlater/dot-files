# hv.sh
#
# Author: Thien-Thi Nguyen <ttn@gnu.org>
# License: Public Domain
##
# Usage: version=VERSION ; . hv.sh
#
# This file is not executable.  Instead, it is meant
# to be sourced (i.e., "." in sh, or "source" in bash).
#
# It sets shell variable ‘me’ to the basename(1) of $0,
# then checks $1 for either ‘--help’ or ‘--version’.
#
# If ‘--help’, it scans $0 for the flush-left comment block w/ form:
#   ##
#   # HELP-TEXT
#   # ...
#   ##
# formats it to stdout, and exits successfully (status 0).  More precisely,
# the first and last comment lines are ‘##’ (double-hash) and are omitted,
# as are the ‘#’ (hash) at the beginning of each line.  HELP-TEXT can be
# multiline, including blank lines.  It's customary to start HELP-TEXT w/
# "Usage:" or "Synopsis:", like a manpage, but that is not required.
#
# If $1 is ‘--version’, this file displays to stdout
#   PROGNAME VERSION
# and exits successfully (status 0).  PROGNAME and VERSION are the values
# of the ‘me’ and ‘version’ shell variables, respectively.  This is why
# ‘version’ must be set prior to sourcing the file.  If ‘version’ is not
# set or is the empty string, display "VERSION UNKNOWN" for VERSION.
#
# Any other value of $1 is silently ignored.
##

me=`basename $0`

if [ x"$1" = x--help ] ; then
    sed '/^##/,/^##/!d;/^##/d;s/^# //g;s/^#$//g' $0
    exit 0
fi

if [ x"$1" = x--version ] ; then
    echo $me ${version:-VERSION UNKNOWN}
    exit 0
fi

# hv.sh ends here
