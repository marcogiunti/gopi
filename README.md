# GoPi
The GoPi compiler transforms high level processes featuring linear and secret channels in executable Go programs.

## Prerequisites
* OCaml
* OCamlbuild
* OCamlfind
* Menhir
* Z3 (Z3Prover/z3)
* Go

## Compilation from source

We assume GNU make, which may be named gmake on your system.

To compile the files, run 

```shell
make
```

## Getting started

To see the syntax of processes and the list of options available, run gopi without arguments

```shell
gopi
```

To compile and run a process in the directory examples, execute

```shell
gopi -t 500 examples/spooler.pi
```

See the man page for more details

```shell
man ./manpage
```


