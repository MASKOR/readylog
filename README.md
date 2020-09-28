ReadyLog was developed for [ECLiPSe Prolog](http://eclipseclp.org/). It may work
with differend Prolog dialects such as SWI, but will probably require some
porting work.

Assuming that the ECLiPSe interpreter is invoked as `eclipse`, the simplified
blocksworld example can be executed by entering the following command in the
main directory of this repository:
```
eclipse -f examples/blocksworld/blocksworld.pl -e 'icp(main)'
```

The theory, background and semantics of the ReadyLog language are covered in
detail in [Alexander Ferrein](http://robotics.fh-aachen.de/~ferrein/)'s
[PhD thesis](http://publications.rwth-aachen.de/record/49943/files/Ferrein_Alexander.pdf)

Note that some features that depend on non-GPL code have been stripped from this version.
Please contact Alexander Ferrein if you need access to a full-featured ReadyLog.
