# Hybrid Systems Simulator (HSS)

> --- With A Monitor for Bounded LTL Properties ---

## Requirements

* CAPD-DynSys-3.0 http://capd.ii.uj.edu.pl/
* OCaml (tested with ver.4.01.0)
* C/C++ (tested with gcc ver.4.8.2)

## Install

1. Apply patch to CAPD-DynSys.
```
$ cd $(CAPD_DIR)
$ patch -p1 
```
2. Build and install CAPD-DynSys.
```
$ cd $(CAPD_DIR)
$ ./configure
$ make install
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

A file `pped.dat` will be generated in the current directory which contains a dumped data of the witness trajectory.
If you have Mathematica, you can use a notebook file `plot.nb` to visualize a dump file.

## Reference

D. Ishii, N. Yonezaki, A. Goldsztejn: Monitoring Bounded LTL Properties Using Interval Analysis (submitted to HSCC'15).
