#include "Wire.h"
#include "ll_lamb.h"

Sexpr_t mop3_Wire_begin(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_Wire_begin()");
  ll_try {
    Bool_t res;
    if (sexpr != NIL) {
      Sexpr_t sx_addr = lamb.car(sexpr);
      if (sx_addr->type() != Cell::T_INT) throw lamb.mk_syserror("%s bad type %d", me, sx_addr->type());
      Int_t addr = sx_addr->as_Int_t();
      res = Wire.begin(addr);
    }
    else res = Wire.begin();
    return lamb.mk_bool(res, env_stack);
  }
  ll_catch();
}

Sexpr_t mop3_Wire_end(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return lamb.mk_bool(Wire.end(), env_stack);
}

Sexpr_t mop3_Wire_requestFrom(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("mop3_Wire_requestFrom()");
  ll_try {
    Sexpr_t addr = lamb.car(sexpr);
    sexpr = lamb.cdr(sexpr);
    Sexpr_t qty  = lamb.car(sexpr);
    sexpr = lamb.cdr(sexpr);
    Bool_t stop = true;
    if (sexpr != NIL) stop = lamb.car(sexpr) != HASHF;
    if (addr->type() != Cell::T_INT) throw lamb.mk_syserror("%s bad type %d", me, addr->type());
    if (qty->type()  != Cell::T_INT) throw lamb.mk_syserror("%s bad type %d", me, qty->type());
    Int_t res = Wire.requestFrom(addr->as_Int_t(), qty->as_Int_t(), (Int_t) stop);
    return lamb.mk_integer(res, env_stack);
  }
  ll_catch();
}

Sexpr_t mop3_Wire_beginTransmission(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

Sexpr_t mop3_Wire_endTransmission(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

Sexpr_t mop3_Wire_write(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

Sexpr_t mop3_Wire_available(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

Sexpr_t mop3_Wire_read(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

Sexpr_t mop3_Wire_setClock(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

Sexpr_t mop3_Wire_onReceive(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

Sexpr_t mop3_Wire_onRequest(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

Sexpr_t mop3_Wire_setWireTimeout(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

Sexpr_t mop3_Wire_clearWireTimeoutFlag(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

Sexpr_t mop3_Wire_getWireTimeoutFlag(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  return OBJ_UNDEF;
}

#define _make_dispatch_(_sym_) { OBJ_UNDEF, #_sym_, mop3_Wire_##_sym_ }
Sexpr_t mop3_Wire(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_Wire()");
  ll_try {
    static struct {
      Sexpr_t sym;
      const char *str;
      Lamb::Mop3st_t func;
    } dispatch[] = {
      _make_dispatch_(begin),
      _make_dispatch_(end),
      _make_dispatch_(requestFrom),
      _make_dispatch_(beginTransmission),
      _make_dispatch_(endTransmission),
      _make_dispatch_(write),
      _make_dispatch_(available),
      _make_dispatch_(read),
      _make_dispatch_(setClock),
      _make_dispatch_(onReceive),
      _make_dispatch_(onRequest),
      _make_dispatch_(setWireTimeout),
      _make_dispatch_(clearWireTimeoutFlag),
      _make_dispatch_(getWireTimeoutFlag)
    };
    const Int_t Nsyms = sizeof(dispatch)/sizeof(dispatch[0]);
      
    static bool initialized = false;
    if (!initialized) {
      for (Int_t i = 0; i < Nsyms; i++)
	dispatch[i].sym = lamb.mk_symbol(dispatch[i].str, env_stack);
      initialized = true;
    }
    
    Sexpr_t command = lamb.car(sexpr);
    for (Int_t i = 0; i < Nsyms; i++)
      if (command == dispatch[i].sym)
	return dispatch[i].func(lamb, lamb.cdr(sexpr), env_stack);
    throw lamb.mk_syserror("%s not a Wire command: %s\n", me, command->dump().c_str());
  }
  ll_catch();
}
#undef _make_dispatch_
//Install all Wire symbols in base environment.
//
Sexpr_t mop3_setup_Wire(Lamb &lamb)
{
  ME("::mop3_setup_Wire()");
  ll_try {

    typedef struct { Lamb::Mop3st_t func;  const char *name;  bool syntax; } StandardProcedure;
    static const StandardProcedure std_procs[] = {
      mop3_Wire, "Wire", false
    };

    const int Nstd_procs = sizeof(std_procs)/sizeof(std_procs[0]);
  
    lamb.printf("%s defining %d Mops\n", me, Nstd_procs);
    Sexpr_t env_exec = lamb.r5_interaction_environment();
    Sexpr_t env_target = lamb.r5_base_environment();
    for (int i=0; i<Nstd_procs; i++) {
      const StandardProcedure &p  = std_procs[i];
      Sexpr_t sym  = lamb.mk_symbol(p.name, env_exec);
      lamb.gc_root_push(sym);
      Sexpr_t proc = p.syntax ? lamb.mk_Mop3_nprocst_t(p.func, env_exec) : lamb.mk_Mop3_procst_t(p.func, env_exec);
      lamb.gc_root_pop();
      lamb.bind_bang(env_target, sym, proc, env_exec);
    }
    return NIL;
  }
  ll_catch();
}
