--- 
tar: |-
  CREATING
  
  To tar and gzip a directory and save the newly created file in the same directory:
  $ tar czf dirnamee.tgz dirname
  
  Another example which
    a) starts from inside the directory being tarred and saves the new file one level up
    b) pipes through gzip instead of using tar's own gzip facility
       (which is handy if you want to use special gzip flags, or use zip instead of gzip)
  $ tar cf - . | gzip > ../dirname.tgz
  
  Here we use the -j flag to use bzip instead of gzip:
  $ tar -cjf home.tar.bz2 home
  
  Here we use long tags:
  $ tar --create --verbose --preserve \
        --ignore-failed-read --file=<file to write to> <files to tar>
  
  
  EXTRACTING
  
  Extract an archive (with decompression)
  $ tar -xvzf myfile.tar.gz
  
  To extract, first change to the location you want the files (ie. create the outermost
  directory of the archive and then cd to it) and then:
  
  $ tar zxvf /some/path/filename.tgz
  (use j instead of z for bzip files)
  
  
  FILE EXTENSIONS
  
  .tgz is equivalent to .tar.gz
  .tbz and .tb2 are equivalent to .tar.bz2
  .taz is equivalent to .tar.Z
  .tlz is equivalent to .tar.lz
  .txz is equivalent to .tar.xz
  
  
  COMMON ARGUMENTS
  
  $ tar -j (--bzip)
  $ tar -v (--verbose)
  $ tar -z (--gzip)
  $ tar -x (--extract)
  $ tar -c (--create)
  $ tar -t (--list)
  $ tar -f (--file)
  $ tar -p (--preserve)
  $ tar --ignore-failed-read
  $ tar --totals (prints total bytes written with --create.
