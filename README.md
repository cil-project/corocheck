# CoroCheck

A static checking and inference tool for coroutine annotations

## Prerequisites

You need the latest [CIL](http://ocaml.org/) snapshot (`develop` branch) and
[ocamlgraph](http://ocamlgraph.lri.fr/) ≤ 1.8.2 (***warning:*** ocamlgraph 1.8.3 has a
[breaking API change](https://github.com/backtracking/ocamlgraph/issues/3)).
To generate pdf graphs, you also need `dot` from
[graphviz](http://www.graphviz.org/).

Note that CIL depends on ocaml, findlib and perl.
To install all dependencies on Debian or Ubuntu:
```
apt-get install perl ocaml ocaml-findlib libocamlgraph-ocaml-dev graphviz
```

Then, download and install the latest CIL snapshot:
```
git clone https://github.com/kerneis/cil
cd cil
./configure
make
make install
```

### Note for OPAM users

If you use [opam](http://opam.ocamlpro.com/), it is **strongly**
recommended to use the following `configure` invocation to install CIL:

```
./configure --prefix=`opam config var prefix`
```

Also note that ocamlgraph has been updated to 1.8.3 in opam. To install 1.8.2,
use:

```
opam install ocamlgraph.1.8.2
```

## Build and installation

```
make all install
```

To test that everything is working correctly:
```
make check
```
This generates `test/inference.pdf`, which contains the annotated call graph
corresponding to `test/inference.c`.

## Usage

Create a test file `test.c`:
```
cat << EOF > test.c
#define coroutine_fn __attribute__((__coroutine_fn__))
#define blocking_fn __attribute__((__blocking_fn__))
void coroutine_fn f();
void blocking_fn g();
void h() { f(); }
void coroutine_fn k() { g(); }
EOF
```

Then use `cilly` with the `corocheck` feature to analyse it:
```
export CIL_FEATURES=corocheck
cilly --save-temps --doCoroCheck --dotFile=test.dot -Wno-attributes -c test.c
```

***Important:*** the flag `--save-temps` is necessary to get complete results;
otherwise, CoroCheck would fail to report some missing annotations. On a related
note, if you `#include` headers containing coroutine functions, you should add
`--fullInferenceGraph` to get complete results (but this will generate a very
hard to read .dot file if you have many functions!).

And generate an annotated call graph:
```
dot -Tpdf -o test.pdf test.dot
```
See `test/inference.c` and `test/Makefile` for a full example.

## Options

```
export CIL_FEATURES=corocheck
cilly --help
```
