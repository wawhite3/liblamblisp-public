#include "ll_lamb.h"
#include "ll_mop3.h"
#include "esp_system.h"
#include "esp_efuse.h"

Sexpr_t mop3_esp32_efuse_mac_get_default(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  byte mac[6];
  esp_efuse_mac_get_default(mac);
  return lamb.mk_bytevector(6, mac, env_stack);
}

Sexpr_t mop3_esp32_efuse_pkg_ver(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return lamb.mk_integer(esp_efuse_get_pkg_ver(), env_stack); }

Sexpr_t mop3_esp32_efuse_read_block(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_esp32_efuse_read_block()");
  ll_try {
    Sexpr_t blk        = lamb.car(sexpr);
    Sexpr_t bit_offset = lamb.cadr(sexpr);
    Sexpr_t Nbits      = lamb.caddr(sexpr);
    mop3_typecheck(blk, T_INT);
    mop3_typecheck(bit_offset, T_INT);
    mop3_typecheck(Nbits, T_INT);
    Sexpr_t res = lamb.mk_bytevector(roundup(Nbits->as_Int_t(), 8)/8, 0);
    Int_t n;
    ByteVec_t elems;
    res->any_bvec_get_info(n, elems);
    
    auto err = esp_efuse_read_block((esp_efuse_block_t) blk->as_Int_t(), elems, bit_offset->as_Int_t(), Nbits->as_Int_t());
    if (err != ESP_OK) res = lamb.mk_integer(err, env_stack);
    return res;
  }
  ll_catch();
}

  
Sexpr_t mop3_esp32_efuse_write_block(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_esp32_efuse_write_block");
  ll_try {
    Sexpr_t blk        = lamb.car(sexpr);
    Sexpr_t bits       = lamb.cadr(sexpr);
    Sexpr_t bit_offset = lamb.caddr(sexpr);
    Sexpr_t Nbits      = lamb.cadddr(sexpr);
    mop3_typecheck(blk, T_INT);
    mop3_typecheck(bit_offset, T_INT);
    mop3_typecheck(Nbits, T_INT);
    ByteVec_t p = 0;
    Int_t i = 0;
    if (bits->type() == Cell::T_INT) {
      i = bits->as_Int_t();
      p = (ByteVec_t) &i;
    }
    else bits->any_bvec_get_info(i, p);
    //DEBT all this is likely wrong
    auto err = esp_efuse_write_block((esp_efuse_block_t) blk->as_Int_t(), bits, bit_offset->as_Int_t(), Nbits->as_Int_t());
    return lamb.mk_integer(err, env_stack);
  }
  ll_catch();
}


Sexpr_t mop3_setup_hal_esp32(Lamb &lamb)
{
  ME("::mop3_setup_hal_esp32()");
  ll_try {

    static struct { Lamb::Mop3st_t func;  const char *name; } std_procs[] = {
      mop3_esp32_efuse_mac_get_default, "esp32-efuse-mac-get-default",
      mop3_esp32_efuse_pkg_ver, "esp32-efuse-pkg-ver",
      mop3_esp32_efuse_read_block, "esp32-efuse-read-block",
      mop3_esp32_efuse_write_block, "esp32-efuse-write-block"
    };

    const int Nstd_procs = sizeof(std_procs)/sizeof(std_procs[0]);
  
    lamb.printf("%s defining %d Mops\n", me, Nstd_procs);
    Sexpr_t env_exec   = lamb.r5_interaction_environment();
    Sexpr_t env_target = env_exec;
    for (int i=0; i<Nstd_procs; i++) {
      const auto &p = std_procs[i];
      Sexpr_t sym   = lamb.mk_symbol(p.name, env_exec);
      lamb.gc_root_push(sym);
      Sexpr_t proc  = lamb.mk_Mop3_procst_t(p.func, env_exec);
      lamb.gc_root_pop();
      lamb.bind_bang(env_target, sym, proc, env_exec);
    }
    return NIL;
  }
  ll_catch();
}
