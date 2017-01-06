# HySIA: Hybrid systems/automata Simulation/verification using Interval Analysis

## Requirements

* CAPD-DynSys-3.0 http://capd.ii.uj.edu.pl/
* OCaml (tested with ver. 3.12–4.02)
* C/C++ (tested with gcc ver. 4.7.4–4.8.4)

Tested on Linux ver. 3.2.0–4.4.39 on amd64 and Mac OS X ver. 10.9.5–10.11.6.

## Install

`$(CAPD_DIR)` or `$(HS_DIR)` represents the path to the unpacked directory of CAPD-DynSys or HySIA, respectively.

1. Apply patch to CAPD-DynSys.
    ```
    $ cd $(CAPD_DIR)
    $ patch -p1 < $(HS_DIR)/tmp/capd_dynsys.20140612.diff
    ```
2. Build and install CAPD-DynSys.
    ```
    $ cd $(CAPD_DIR)
    $ ./configure
    $ sudo make install
    ```
3. Build HySIA.
    ```
    $ cd $(HS_DIR)
    $ make
    ```

Binary file `$(HS_DIR)/src_ocaml/hss.opt` will be generated.

## Example

Example HA models are in the `$(HS_DIR)/examples` directory.

```
$ cd $(HS_DIR)
$ ./src_ocaml/hss.opt ./examples/bb-movingtable.ha -a
...
true
```
The option `-a` tells to decide the simulation length automatically according to the BLTL property.
You can also use the option `-n N` (where `N` is a positive integer) to specify the number of steps to simulate.

A file `pped.dat` will be generated in the current directory which contains a dumped data of the witness trajectory.
If you have Mathematica, you can use a notebook file `plot.nb` to visualize a dump file.

## Reference

A. Goldsztejn, D. Ishii: A Parallelotope Method for Hybrid System Simulation. Reliable Computing, 23:163–185, 2016.

D. Ishii, N. Yonezaki, A. Goldsztejn. Monitoring Temporal Properties using Interval Analysis. IEICE Transactions on Fundamentals of Electronics, Communications and Computer Sciences, E99-A(2):442–453, 2016.

D. Ishii, N. Yonezaki, A. Goldsztejn: Monitoring Bounded LTL Properties Using Interval Analysis, NSV'15.

## License

This software is released under the MIT License, see LICENSE.txt.
