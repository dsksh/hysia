# HySIA: Hybrid systems/automata Simulation using Interval Analysis

> --- With A Monitor for Bounded LTL Properties ---

## Requirements

* CAPD-DynSys-3.0 http://capd.ii.uj.edu.pl/
* OCaml (tested with ver. 3.12.1 and ver. 4.01.0)
* C/C++ (tested with gcc ver. 4.7.4 and ver. 4.8.2)

Tested on Linux ver. 3.2.0-4-amd64 and Mac OS X ver. 10.9.5.

## Install

`$(CAPD_DIR)` or `$(HSS_DIR)` represents the path to the unpacked directory of CAPD-DynSys or HSS, respectively.

1. Apply patch to CAPD-DynSys.
```
$ cd $(CAPD_DIR)
$ patch -p1 < $(HSS_DIR)/tmp/capd_dynsys.20140612.diff
```
2. Build and install CAPD-DynSys.
```
$ cd $(CAPD_DIR)
$ ./configure
$ sudo make install
```
3. Build HSS.
```
$ cd $(HSS_DIR)
$ make
```

Binary file `$(HSS_DIR)/src_ocaml/hss.opt` will be generated.

## Example

Example HA models are in the `$(HSS_DIR)/examples` directory.

```
$ cd $(HSS_DIR)
$ ./src_ocaml/hss.opt ./examples/bb-movingtable.ha -a
...
true
```
The option `-a` tells to decide the simulation length automatically according to the BLTL property.
You can also use the option `-n N` (where `N` is a positive integer) to specify the number of steps to simulate.

A file `pped.dat` will be generated in the current directory which contains a dumped data of the witness trajectory.
If you have Mathematica, you can use a notebook file `plot.nb` to visualize a dump file.

## Reference

D. Ishii, N. Yonezaki, A. Goldsztejn: Monitoring Bounded LTL Properties Using Interval Analysis, NSV'15.
