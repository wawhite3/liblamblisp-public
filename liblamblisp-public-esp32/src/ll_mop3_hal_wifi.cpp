#include "WiFi.h"
#include "ll_lamb.h"
#include "ll_mop3.h"

Sexpr_t mop3_wifi_begin(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_wifi_begin()");
  ll_try {
    Sexpr_t ssid = lamb.car(sexpr);
    Sexpr_t pwd  = lamb.cadr(sexpr);
    Int_t status = WiFi.begin(ssid->any_str_get_chars(), pwd->any_str_get_chars());
    return lamb.mk_integer(status, env_stack);
  }
  ll_catch();
}

Sexpr_t mop3_wifi_disconnect(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  Sexpr_t res = OBJ_UNDEF;
  Bool_t wifioff = false;
  Bool_t eraseap = false;
  
  if (sexpr != NIL) {
    if (lamb.car(sexpr) != HASHF) wifioff = true;
    sexpr = lamb.cdr(sexpr);
  }
  if (sexpr != NIL)
    if (lamb.car(sexpr) != HASHF) eraseap = true;
  return lamb.mk_bool(WiFi.disconnect(wifioff, eraseap), env_stack);
}

Sexpr_t mop3_wifi_config(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_wifi_config()");
  ll_try {
    Sexpr_t sx_ip      = OBJ_UNDEF;
    Sexpr_t sx_dns     = OBJ_UNDEF;
    Sexpr_t sx_gateway = OBJ_UNDEF;
    Sexpr_t sx_subnet  = OBJ_UNDEF;
    if (sexpr == NIL) throw lamb.mk_syserror("%s IP address required", me);
    sx_ip = lamb.car(sexpr);
    sexpr = lamb.cdr(sexpr);
    if (sexpr != NIL) {	sx_dns     = lamb.car(sexpr);  sexpr = lamb.cdr(sexpr); }
    if (sexpr != NIL) { sx_gateway = lamb.car(sexpr);  sexpr = lamb.cdr(sexpr); }
    if (sexpr != NIL) { sx_subnet  = lamb.car(sexpr);  sexpr = lamb.cdr(sexpr); }
    //DEBT what to do with those things
    return OBJ_UNDEF;
  }
  ll_catch();
}

Sexpr_t mop3_wifi_setDNS(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  Sexpr_t res = OBJ_UNDEF;
  return res;
}

Sexpr_t mop3_wifi_SSID(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return lamb.mk_string(env_stack, WiFi.SSID().c_str()); }
Sexpr_t mop3_wifi_BSSID(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return lamb.mk_string(env_stack, WiFi.BSSIDstr().c_str()); }
Sexpr_t mop3_wifi_RSSI(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return lamb.mk_integer(WiFi.RSSI(), env_stack); }
Sexpr_t mop3_wifi_scanNetworks(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return lamb.mk_integer(WiFi.scanNetworks(), env_stack); }
Sexpr_t mop3_wifi_localIP(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return lamb.mk_string(env_stack, WiFi.localIP().toString().c_str()); }
Sexpr_t mop3_wifi_subnetMask(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return lamb.mk_string(env_stack, WiFi.subnetMask().toString().c_str()); }
Sexpr_t mop3_wifi_gatewayIP(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return lamb.mk_string(env_stack, WiFi.gatewayIP().toString().c_str()); }
Sexpr_t mop3_wifi_getSocket(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return lamb.mk_string(env_stack, "<WiFi.getSocket() unsupported>"); }

Sexpr_t mop3_wifi_encryptionType(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_wifi_encryptionType()");
  ll_try {
    byte ifc = 0;
    if (sexpr != NIL) {
      Sexpr_t sx_ifc = lamb.car(sexpr);
      if (sx_ifc->type() != Cell::T_INT) throw lamb.mk_syserror("%s bad type %d", me, sx_ifc->type());
      ifc = (byte) sx_ifc->as_Int_t();
    }
    return lamb.mk_integer(WiFi.encryptionType(ifc), env_stack);
  }
  ll_catch();
}

Sexpr_t mop3_wifi_macAddress(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  byte mac[6];
  WiFi.macAddress(mac);

  String rstr = "";
  String colon = "";
  for (int i=0; i<6; i++) {
    rstr = toString("%s%s%02x", rstr.c_str(), colon.c_str(), mac[i] & 0xff);
    colon = ":";
  }
  return lamb.mk_string(env_stack, rstr.c_str());
}

Sexpr_t mop3_wifi_status(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  Int_t stat   = WiFi.status();
  Charst_t msg = "WL_UNKNOWN";
  switch (stat) {
  case WL_NO_SHIELD:		msg = "WL_NO_SHIELD";		break;
  case WL_IDLE_STATUS:		msg = "WL_IDLE_STATUS";		break;
  case WL_NO_SSID_AVAIL:	msg = "WL_NO_SSID_AVAIL";	break;
  case WL_SCAN_COMPLETED:	msg = "WL_SCAN_COMPLETED";	break;
  case WL_CONNECTED:		msg = "WL_CONNECTED";		break;
  case WL_CONNECT_FAILED:	msg = "WL_CONNECT_FAILED";	break;
  case WL_CONNECTION_LOST:	msg = "WL_CONNECTION_LOST";	break;
  case WL_DISCONNECTED:		msg = "WL_DISCONNECTED";	break;
  }

  Sexpr_t res = NIL;
  Sexpr_t sx_msg  = lamb.mk_string(env_stack, msg);
  res = lamb.cons(sx_msg, res, env_stack);
  Sexpr_t sx_stat = lamb.mk_integer(stat, env_stack);
  res = lamb.cons(sx_stat, res, env_stack);
  return res;
}

//DEBT this will be better done with an object in lisp.
Sexpr_t mop3_WiFi(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_wifi()");
  ll_try {
    static struct {
      Sexpr_t sym;
      const char *str;
      Lamb::Mop3st_t func;
    } dispatch[] = {
      { OBJ_UNDEF, "begin", mop3_wifi_begin },
      { OBJ_UNDEF, "disconnect", mop3_wifi_disconnect },
      { OBJ_UNDEF, "config", mop3_wifi_config },
      { OBJ_UNDEF, "setDNS", mop3_wifi_setDNS },
      { OBJ_UNDEF, "SSID", mop3_wifi_SSID },
      { OBJ_UNDEF, "BSSID", mop3_wifi_BSSID },
      { OBJ_UNDEF, "RSSI", mop3_wifi_RSSI },
      { OBJ_UNDEF, "encryptionType", mop3_wifi_encryptionType },
      { OBJ_UNDEF, "scanNetworks", mop3_wifi_scanNetworks },
      { OBJ_UNDEF, "status", mop3_wifi_status },
      { OBJ_UNDEF, "getSocket", mop3_wifi_getSocket },
      { OBJ_UNDEF, "macAddress", mop3_wifi_macAddress },
      { OBJ_UNDEF, "localIP", mop3_wifi_localIP },
      { OBJ_UNDEF, "subnetMask", mop3_wifi_subnetMask },
      { OBJ_UNDEF, "gatewayIP", mop3_wifi_gatewayIP }
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
    throw lamb.mk_syserror("%s not a WiFi command: %s\n", me, command->dump().c_str());
  }
  ll_catch();
}

//Install all WiFi symbols in base environment.
//
Sexpr_t mop3_setup_WiFi(Lamb &lamb)
{
  ME("::mop3_setup_WiFi()");
  ll_try {
    typedef struct { Lamb::Mop3st_t func;  const char *name;  bool syntax; } StandardProcedure;
    static const StandardProcedure std_procs[] = {
      mop3_WiFi, "WiFi", false
    };

    const int Nstd_procs = sizeof(std_procs)/sizeof(std_procs[0]);
  
    lamb.printf("%s defining %d Mops\n", me, Nstd_procs);
    Sexpr_t env_exec   = lamb.r5_interaction_environment();
    Sexpr_t env_target = lamb.r5_base_environment();
    for (int i=0; i<Nstd_procs; i++) {
      const StandardProcedure &p = std_procs[i];
      Sexpr_t sym = lamb.mk_symbol(p.name, env_exec);
      lamb.gc_root_push(sym);
      Sexpr_t proc = p.syntax ? lamb.mk_Mop3_nprocst_t(p.func, env_exec) : lamb.mk_Mop3_procst_t(p.func, env_exec);
      lamb.gc_root_pop();
      lamb.bind_bang(env_target, sym, proc, env_exec);
    }
    return NIL;
  }
  ll_catch();
}
