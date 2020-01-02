This is test Tk GUI for the [AZip](https://azip.sourceforge.io/)


## Build the program from sources

To build you need:

* compiler - GCC with enabled Ada support or (best option) GNAT from:

  https://www.adacore.com/download/

  It is recommended to use GNAT GPL 2019 to compile the program.
  The program does not work with old compilers (like GCC 4.9) since it
  lacks full support for Ada 2012.

* Tcl/Tk library. Should be available in every Linux distribution. For
  Windows it is recommended to use MagicSplat version:

  https://www.magicsplat.com/tcl-installer/index.html

* TASHY library. You can get it from:

   https://github.com/thindil/tashy

   **Important:** To build this version of AZip you will need a repository
   version of the library, not release (probably soon or later it will
   be better synchronized).

If you have all the required packages, navigate to the main directory(where
this file is) to compile:

* Easiest way to compile the program is use Gnat Programming Studio included
  in GNAT. Just run GPS, select *azipgtk.gpr* as a project file and select
  option `Build All`.

* If you prefer using console: in main source code directory type
  `gprbuild -P aziptk.gpr` for debug mode build or for release mode:
  `gprbuild -P aziptk.gpr -XMode=release`.

## Running it

Just run `aziptk` (on Windows `aziptk.exe`) binary in `bin` directory.
