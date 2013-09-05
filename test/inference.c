#define coroutine_fn __attribute__((__coroutine__))
#define blocking_fn  __attribute__((__blocking__))

/* Roots */
coroutine_fn extern void f();
blocking_fn extern void g();
void h() { };

/* spurious coroutine_fn */
coroutine_fn void f1() { }

/* coroutine_fn ok */
coroutine_fn void f2() { f(); }

/* missing coroutine_fn */
void f3() { f(); }

/* missing coroutine_fn but ok because f1 is spurious */
void f4() { f1(); }

/* spurious coroutine_fn, but in fact ok because f3 is missing */
coroutine_fn void f5() { f3(); }

/* Deeper, just to check it works */
coroutine_fn void f6() { f5(); }
void f7() { f4(); }

/* blocking_fn ok */
blocking_fn void g1() { g(); }

/* conflicting annotation */
blocking_fn coroutine_fn void g2() { }

/* blocking_fn inferred as coroutine_fn */
blocking_fn void g3() { f(); }

/* blocking_fn called from native: ok */
void g4() { g(); }

/* blocking_fn called from coroutine_fn: bad */
coroutine_fn void f8() { f(); g(); g1(); }

/* blocking_fn called from spurious coroutine_fn: suspicious */
coroutine_fn void f9() { g(); }

/* blocking_fn called from missing coroutine_fn: suspicious */
void f10() { f(); g(); }

/* coroutine_fn function pointer */
typedef coroutine_fn void coop(void);
typedef void nativefun(void);
coop *p = f;

/* missing coroutine_fn here */
void f11() { p(); }

/* not spurious, because a pointer is taken so we should trust the annotation, just like extern functions */
coroutine_fn void f12() { h(); }
coop *p2 = f12;

/* Checking wrong (implicit) casts */
struct s {
  void coroutine_fn (*f)(void);
  void coroutine_fn (*g)(void);
  void (*h)(void);
};

void coroutine_fn a(void);
void b(void);

struct s foo = {
  .f = a,   /* OK */
  .g = b,   /* Bad */
  .h = a,   /* Bad  */
};

coop *p3 = a; /* OK */
nativefun *p4 = a; /* Bad */

coop *f13() {
  return &a;
}

void f14() {
  nativefun *p = a; /* Bad */
  coop *p2 = b;     /* Bad */

  p = f13(); /* Bad */
  p2 = f13(); /* OK */
}
