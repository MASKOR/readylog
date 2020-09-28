ReadyLog was developed for [ECLiPSe Prolog](http://eclipseclp.org/). It may work
with differend Prolog dialects such as SWI, but will probably require some
porting work.

Assuming that the ECLiPSe interpreter is invoked as `eclipse`, the simplified
blocksworld example can be executed by entering the following command in the
main directory of this repository:
```
eclipse -f examples/blocksworld/blocksworld.pl -e 'icp(main)'
```
