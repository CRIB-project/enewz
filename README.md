# What’s ENEWZ

ENEWZ is an energy loss calculator which works on command line, and was originally written by Y. Watanabe san (in 90s!) in FORTRAN based on J. Ziegler’s code. (Hayakawa fixed minor bugs and usability.)

# How to install
* Move to the directory where you cloned the source files (for example, /usr/local/enewz)
* Correct the path written in a file DATAPATH.INC to where it locates, if it is different from /usr/local/enewz/ .

Then
```
$ make
$ sudo make install
```

# How to use on command line
After completing “make install”, type “enewz” on the command line, then you can calculate energy loss by putting in parameters like A, Z, E and material information. (Check if the results are not so different from LISE++’s calculations.)

There are several different commands:
* enewz: calculates energy after materials.
* eoldz: calculates energy before materials.
* rangez: calculates a range from an energy.
* range2ez: calculates an enegy from a range.
* stest, stopping… I have never used them, but you may try.

# How to use in C++ codes / ROOT macros
(Information originally found in daid’s website)

If you succeed “make install”, a static library libenewzlib.a will be created on /usr/local/lib/ . This allows you to use enewz subrotine (and others such as eoldzsub, rangezsub, range2ezsub, etc.) in C++ codes.

There are both subrotines named **enewzsub** and **enewzsub_etot**, etc. The input and output energies of the former are treated in **MeV/u**, while those of the latter in **MeV**. 

There is a sample macro (test_enewz.C) and a makefile (Makefile_test) to demonstrate how to use enewz in C++. The makefile automatically determines the OS (Linux or Mac, I did not test it on Windows). You might need to change the path of “libstdc++.a” in the makefile. 

There would be an error when you try to make it on Mac (on Ubuntu I did not get such an error.) 

```
gfortran: error: unrecognized command-line option '-stdlib=libc++'
```

“-stdlib=libc++” is included in $ root-config --libs . If you change linking library “-stdlib=libc++” to “-lstdc++ -lc++”, the compilation may work. Please modify the makefile according to your environment. 





--------------------------------------------------------------------------------------------
# History...

enewz  (Sep. 9, 2002)

- Given by Yutaka Watanabe in 1998

- SNKE_MATTER.DAT updated in 2002 (T. Teranishi)

- Debaged eoldz (S. Hayakawa)

- Added *sub*.f files to define subroutines to be included in libenewzlib.a (S. Hayakawa)

- SNKE_MATTER.DAT updated in 2022 (S. Hayakawa)

-----------------------------------------------------------------------------
From:	MX%"yutaka@ipparkia.riken.go.jp"  "Watanabe Yutaka" 15-DEC-1998 14:34:45.41
To:	MX%"teranishi@rikaxp.riken.go.jp"
CC:	MX%"yutaka@ipparkia.riken.go.jp"
Subj:	enew

 こんにちは、渡辺＠石原研究室 です。

 enew の linux 版と ziegler のコードを入れた enewz を
ftp://ipparkia.riken.go.jp/home/yutaka/pub/enew.tgz
ftp://ipparkia.riken.go.jp/home/yutaka/pub/enewz.tgz
に置きました。

make でコンパイルできます。
enewz のコンパイルは DATAPATH.INC に書かれている path を
ソースのあるディレクトリに変更してください。

enew の使い方は vax のものとおなじです。

enewz のインターフェースは enew に似せて作りましたが、
matter list にない物質の入れ方が異なります。
物質を構成する組成を入れてください。
例)
  メタン CH4
   
   Input : Z, Number of element in one molecule (END = -1 -1.)
     > 
6 1
   Input : Z, Number of element in one molecule (END = -1 -1.)
     > 
1 4
   Input : Z, Number of element in one molecule (END = -1 -1.)
     > 
-1 -1

 以上、よろしくお願い致します。

                                        東京大学大学院 理学系研究科
                                        物理学専攻 石原研究室 Ｄ３
                                                渡辺 裕
