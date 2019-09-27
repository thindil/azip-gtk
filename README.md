This is test GTKAda GUI for the [AZip](https://azip.sourceforge.io/)


## Build the program from sources

To build you need:

* compiler - GCC with enabled Ada support or (best option) GNAT from:

  https://www.adacore.com/download/

  It is recommended to use GNAT GPL 2019 to compile the program.
  The program does not work with old compilers (like GCC 4.9) since it
  lacks full support for Ada 2012.

* GtkAda library which should be available in most Linux distributions. Best
  option is to use (with GNAT GPL) AdaCore version of GtkAda from:

  https://www.adacore.com/download/more

  At this moment tested version of GtkAda is 2019 and the program require GTK
  library in version 3.14 (may not works with other versions).

If you have all the required packages, navigate to the main directory(where
this file is) to compile:

* Easiest way to compile the program is use Gnat Programming Studio included
  in GNAT. Just run GPS, select *azip-gtk.gpr* as a project file and select
  option `Build All`.

* If you prefer using console: in main source code directory type
  `gprbuild -P azip-gtk.gpr` for debug mode build or for release mode:
  `gprbuild -P azip-gtk.gpr -XMode=release`.

## Running it

When you trying to run build by yourself version of the program, use script
`run.sh`. The program will not works if you try to start it by binary file
`azip-gtk` from `bin` directory.
