# CoroCheck

A static checking and inference tool for coroutine annotations

## Prerequisites

You need the latest [CIL](http://ocaml.org/) snapshot (`develop` branch) and
[ocamlgraph](http://ocamlgraph.lri.fr/) ≤ 1.8.2 ( ***warning:*** ocamlgraph 1.8.3 has a
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

If you do not wish to install CIL and CoroCheck globally (note that both
of them provide a `make uninstall` target), see [below the instructions
for QEMU](#qemu) for an example of how to compile and use corocheck
without installing them.

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

## Small example

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

## QEMU

CoroCheck can be used to check the `coroutine_fn` annotations used in
[QEMU](http://wiki.qemu.org/). Assuming you have installed the
dependencies listed in [prerequisites](#prerequisites), here is how to
proceed to build QEMU for target `x86_64-softmmu` with CoroCheck
warnings and build pdfs of annotated callgraphs.

Look at `nbd.dot.pdf` for a small yet interesting example.

```
export ROOTDIR=$(pwd)
git clone https://github.com/kerneis/cil
git clone https://github.com/kerneis/corocheck
git clone https://github.com/qemu/qemu
cd cil
./configure
make
cd ../corocheck
make all OCAMLPATH=$ROOTDIR/cil/lib
cd ../qemu
patch -p1 << EOF
diff --git a/include/block/coroutine.h b/include/block/coroutine.h
index 4232569..3bafa4e 100644
--- a/include/block/coroutine.h
+++ b/include/block/coroutine.h
@@ -44,7 +44,7 @@
  *       ....
  *   }
  */
-#define coroutine_fn
+#define coroutine_fn __attribute__((coroutine_fn))
 
 typedef struct Coroutine Coroutine;
 
EOF
mkdir -p bin/corocheck
cd bin/corocheck
../../configure \
  --disable-werror --target-list=x86_64-softmmu \
  --with-coroutine=ucontext    \
  --cc="$ROOTDIR/cil/bin/cilly" \
  --extra-cflags="\
    -U__SSE2__ -w \
    --load=$ROOTDIR/corocheck/_build/corocheck.cma \
    --save-temps --noMakeStaticGlobal --useLogicalOperators \
    --useCaseRange --doCoroCheck"
make 2>&1 | tee make.log | grep ^Warning:
find . -name "*.dot" -exec dot -Tpdf -o {}.pdf {} \;
```

Note: you should be able to build any QEMU target, but we test mainly
with this one for the moment. Please report any bugs. Thanks!
