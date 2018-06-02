#!/usr/bin/python3

# Script to download the Ada Reference Manual and its formatting tool.

# Copyright (c) 2010, 2012 Stephen Leake <stephen_leake@stephe-leake.org>
# Copyright (c) 2013       Nicolas Boulenguez <nicolas@debian.org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

import os.path
import re
import urllib.request
import sys

USE_EXISTING_FILES = True

######################################################################
class CVSWeb ():

    def __init__ (self, hostname, top_directory):
        # Instantiation requires distant root.

        self._host = 'http://' + hostname
        self._cgi = '/cgi-bin/cvsweb.cgi/' + top_directory + '/'

    def files_in_directory (self, path):
        # List text files in the head revision of a directory.

        # I did not find a way to list a tagged revision, except a
        # direct tag list for each file.

        cgi = self._cgi + str.lower (path) + '/'
        with urllib.request.urlopen (self._host + cgi) as f:
            contents = f.read ()
        # Refuse directory names (with a trailing slash).
        # Ignore case of the displayed file name.
        pattern = '<a href="' + re.escape (cgi) + '([^"/]+)">(\\1)</a><br>'
        matches = re.finditer (str.encode (pattern), contents, re.IGNORECASE)
        return (bytes.decode (m.group (2)) for m in matches)

    def download_file (self, path,
                       tag = None): # None means the head revision.
        # Return the contents of the tagged revision of path, as bytes.
        # If tag is provided, but no revision of this file carries it,
        # None is returned.

        cgi = self._cgi + str.lower (path)
        if tag != None:
            with urllib.request.urlopen (self._host + cgi) as f:
                contents = f.read ()
            pattern = '<a href="' + re.escape (cgi) \
                + '\?rev=([0-9.]+)">\\1</a></b></span><br>\r\n' \
                + 'Modified <i>[^<]*</i>\r\n' \
                + 'by <i>[^<]*</i>( with line changes <i>[^<]*</i>)?<br>\r\n' \
                + 'CVS Tags: <b>[^<]*' + re.escape (tag) + '[^<]*</b><br>'
            match = re.search (str.encode (pattern), contents)
            if not match:
                return None
            cgi += '?rev=' + bytes.decode (match.group (1))
        else:
            cgi += '?rev=HEAD'
        with urllib.request.urlopen (self._host + cgi) as f:
            contents = f.read ()
        return contents

######################################################################
def download_subdir (cvsweb, subdir,
                     tag = None,       # None means the head revision.
                     rename_lowercase = False,
                     strip_carriage_returns = False):

    def fmt (a, b):
        print ('{:<30} : {}'.format (a, b))

    fmt ('Subdirectory', subdir)
    if tag:
        fmt ('Revision', tag)
    else:
        fmt ('Revision', 'latest')
    fmt ('Renaming files to lowercase', rename_lowercase)
    fmt ('Stripping carriage returns', strip_carriage_returns)

    try:
        os.mkdir (subdir) # Fails if the directory exists.
    except FileExistsError:
        if not USE_EXISTING_FILES:
            raise

    for basename in cvsweb.files_in_directory (subdir):
        src = subdir + '/' + basename
        if rename_lowercase:
            basename = str.lower (basename)
        dst = os.path.join (subdir, basename) # Local path join.
        if USE_EXISTING_FILES and os.path.exists (dst):
            fmt (dst, 'using existing file')
            continue
        contents = cvsweb.download_file (src, tag)
        if not contents:
            fmt (dst, 'no such tag')
            continue
        if strip_carriage_returns:
            contents = re.sub (b'\r\n', b'\n', contents)
        with open (dst, 'bw') as o:
            o.write (contents)
        fmt (dst, 'downloaded')

######################################################################

cvsweb = CVSWeb (hostname = 'www.ada-auth.org',
                 top_directory = 'arm')
tags = { '1995':'Final_TC1',
         '2005':'Amend_Final',
         '2012':'Ada2012_TC1' }

if len (sys.argv) == 2 and sys.argv [1] == 'progs':
    download_subdir (cvsweb, 'progs',
                     rename_lowercase = True)
    print ("""
No certification or signature used for downloaded code sources.
Hint: diff -rN --ignore-file-name-case --strip-trailing-cr old/ progs/
""")

elif len (sys.argv) == 2 and sys.argv [1] in tags:
    download_subdir (cvsweb, 'source',
                     tag = tags [sys.argv [1]],
                     rename_lowercase = True,
                     strip_carriage_returns = True)

else:
    print ('Usage: {} [progs | {}]'.format (
            sys.argv [0],
            str.join (' | ', sorted ([str (year) for year in tags.keys ()]))))
