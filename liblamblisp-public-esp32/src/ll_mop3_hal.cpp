#include "ll_lamb.h"


#if 0
Ethernet Class

Ethernet.begin()
Ethernet.dnsServerIP()
Ethernet.gatewayIP()
Ethernet.hardwareStatus()
Ethernet.init()
Ethernet.linkStatus()
Ethernet.localIP()
Ethernet.MACAddress()
Ethernet.maintain()
Ethernet.setDnsServerIP()
Ethernet.setGatewayIP()
Ethernet.setLocalIP()
Ethernet.setMACAddress()
Ethernet.setRetransmissionCount()
Ethernet.setRetransmissionTimeout()
Ethernet.setSubnetMask()
Ethernet.subnetMask()

IPAddress Class

IPAddress()

Server Class

Server
EthernetServer()
server.begin()
server.accept()
server.available()
server.write()
server.print()
server.println()

Client Class

Client
EthernetClient()
client.connected()
client.connect()
client.localPort()
client.remoteIP()
client.remotePort()
client.setConnectionTimeout()
client.write()
print()
client.println()
client.available()
client.read()
client.flush()
client.stop()

EthernetUDP Class

EthernetUDP.begin()
EthernetUDP.read()
EthernetUDP.write()
EthernetUDP.beginPacket()
EthernetUDP.endPacket()
EthernetUDP.parsePacket()
EthernetUDP.available()

UDP class

UDP.stop()
UDP.remoteIP()
UDP.remotePort()
      

Arduino data types and constants.
Constants
Floating Point Constants

Integer Constants
LED_BUILTIN

#endif
//
//Hardware Abstraction Layer
//

Sexpr_t mop3_lamb_reboot(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ lamb.platform_reboot(); return NIL; }
Sexpr_t mop3_lamb_platform_name(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return lamb.mk_string(env_stack, lamb.platform_name()); }
Sexpr_t mop3_lamb_free_heap(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return lamb.mk_integer(lamb.platform_free_heap(), env_stack); }
Sexpr_t mop3_lamb_free_stack(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return lamb.mk_integer(lamb.platform_free_stack(), env_stack); }

Sexpr_t mop3_lamb_random(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  Int_t i;
  lamb.platform_rand((byte *) &i, sizeof(i));
  return lamb.mk_integer(i, env_stack);
}

Sexpr_t mop3_hal_digitalRead(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_hal_digitalRead()");
  Sexpr_t sx_pin = lamb.car(sexpr);
  mop3_typecheck(sx_pin, T_INT);
  return lamb.mk_bool(digitalRead(sx_pin->as_Int_t()) != 0, env_stack);
}

Sexpr_t mop3_hal_digitalWrite(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_hal_digitalWrite()");			//System macro for exception unwinding
  Sexpr_t sx_pin = lamb.car(sexpr);			//Get the pin # in S-expression form
  Sexpr_t val = lamb.cadr(sexpr);			//Get the value in S-expression form
  mop3_typecheck(sx_pin, T_INT);			//System macro to verify S-expression type before trusting contents

  //Extract the C-level integer pin # and write the value
  digitalWrite(sx_pin->as_Int_t(), val != HASHF);	//No check on value; in Scheme, any value "not false" is "true"
  return val;
}

Sexpr_t mop3_hal_pinMode(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_hal_pinMode()");
  Sexpr_t sx_pin = lamb.car(sexpr);
  Sexpr_t mode   = lamb.cadr(sexpr);
  mop3_typecheck(sx_pin, T_INT);
  mop3_typecheck(mode, T_INT);
  pinMode(sx_pin->as_Int_t(), mode->as_Int_t());
  return mode;
}

Sexpr_t mop3_hal_analogRead(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_hal_analogRead()");
  Sexpr_t sx_pin = lamb.car(sexpr);
  mop3_typecheck(sx_pin, T_INT);
  Int_t res = analogRead(sx_pin->as_Int_t());
  return lamb.mk_integer(res, env_stack);
}

Sexpr_t mop3_hal_analogWrite(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_hal_analogWrite()");
  Sexpr_t sx_pin = lamb.car(sexpr);
  Sexpr_t val    = lamb.cadr(sexpr);
  mop3_typecheck(sx_pin, T_INT);
  mop3_typecheck(val, T_INT);
  analogWrite(sx_pin->as_Int_t(), val->as_Int_t());
  return val;
}

Sexpr_t mop3_hal_delay_ms(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_hal_delay_us()");
  Sexpr_t sx_t = lamb.car(sexpr);
  mop3_typecheck(sx_t, T_INT);
  delay(sx_t->as_Int_t());
  return HASHT;
}

Sexpr_t mop3_hal_delay_us(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_hal_delay_us()");
  Sexpr_t sx_t = lamb.car(sexpr);
  mop3_typecheck(sx_t, T_INT);
  delayMicroseconds(sx_t->as_Int_t());
  return HASHT;
}

Sexpr_t mop3_hal_micros(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return lamb.mk_integer(micros(), env_stack); }
Sexpr_t mop3_hal_millis(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return lamb.mk_integer(millis(), env_stack); }

Sexpr_t mop3_hal_analogReadResolution(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_analogReference(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_analogWriteResolution(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_noTone(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_pulseIn(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_pulseInLong(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_shiftIn(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_shiftOut(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_tone(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_random(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_randomSeed(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_bit(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_bitClear(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_bitRead(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_bitSet(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_bitWrite(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_highByte(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_lowByte(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)			{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_attachInterrupt(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_detachInterrupt(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_digitalPinToInterrupt(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)	{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_interrupts(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return OBJ_UNDEF; }
Sexpr_t mop3_hal_noInterrupts(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)		{ return OBJ_UNDEF; }

Sexpr_t mop3_hal_neopixelWrite(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_hal_neopixelWrite()");
  ll_try {
    Int_t args[4];
    for (int i=0; i<4; i++) {
      Sexpr_t arg = lamb.car(sexpr);
      mop3_typecheck(arg, T_INT);
      args[i]     = arg->as_Int_t();
      sexpr       = lamb.cdr(sexpr);
    }
    neopixelWrite(args[0], args[1], args[2], args[3]);
    return OBJ_VOID;
  }
  ll_catch();
}

Sexpr_t mop3_hal_loop_elapsed_us(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack) { return lamb.mk_integer(lamb.platform_loop_elapsed_us(), env_stack);  }
Sexpr_t mop3_hal_loop_elapsed_ms(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack) { return lamb.mk_integer(lamb.platform_loop_elapsed_ms(), env_stack);  }

Sexpr_t mop3_setup_hal(Lamb &lamb)
{
  ME("::mop3_setup_hal()");

  ll_try {
    static const struct {
      Lamb::Mop3st_t func;
      const char *name;
    } hal_bindings[] = {

#define _make_halbinding_(_sym_) { mop3_hal_##_sym_, #_sym_ }
      _make_halbinding_(digitalRead),
      _make_halbinding_(digitalWrite),
      _make_halbinding_(pinMode),
      _make_halbinding_(analogRead),
      _make_halbinding_(analogReadResolution),
      _make_halbinding_(analogReference),
      _make_halbinding_(analogWrite),
      _make_halbinding_(analogWriteResolution),
      _make_halbinding_(noTone),
      _make_halbinding_(pulseIn),
      _make_halbinding_(pulseInLong),
      _make_halbinding_(shiftIn),
      _make_halbinding_(shiftOut),
      _make_halbinding_(tone),
      _make_halbinding_(delay_ms),
      _make_halbinding_(delay_us),
      _make_halbinding_(micros),
      _make_halbinding_(millis),
      _make_halbinding_(random),
      _make_halbinding_(randomSeed),
      _make_halbinding_(bit),
      _make_halbinding_(bitClear),
      _make_halbinding_(bitRead),
      _make_halbinding_(bitSet),
      _make_halbinding_(bitWrite),
      _make_halbinding_(highByte),
      _make_halbinding_(lowByte),
      _make_halbinding_(attachInterrupt),
      _make_halbinding_(detachInterrupt),
      _make_halbinding_(digitalPinToInterrupt),
      _make_halbinding_(interrupts),
      _make_halbinding_(noInterrupts),
      _make_halbinding_(neopixelWrite),
#undef _make_halbinding_

      mop3_hal_loop_elapsed_us, "lamb-loop-elapsed-us",
      mop3_hal_loop_elapsed_ms, "lamb-loop-elapsed-ms"
      
    };
    
    const Int_t Nsyms = sizeof(hal_bindings)/sizeof(hal_bindings[0]);

    lamb.printf("%s defining %d Mops\n", me, Nsyms);
    for (int i=0; i<Nsyms; i++) {
      auto p = hal_bindings[i];
      Sexpr_t sym  = lamb.mk_symbol(p.name, NIL);
      Sexpr_t proc = lamb.mk_Mop3_procst_t(p.func, sym);
      lamb.bind_bang(lamb.r5_interaction_environment(), sym, proc, NIL);
    }
    
    static const struct { int val; const char *name; }
      INT_constants[] = {

#define _make_intbinding_(_sym_) { _sym_, #_sym_ }
      _make_intbinding_(HIGH),
      _make_intbinding_(LOW),
      _make_intbinding_(INPUT),
      _make_intbinding_(INPUT_PULLUP),
      _make_intbinding_(OUTPUT)
#undef _make_intbinding_

    };
    
    const Int_t Nconst = sizeof(INT_constants)/sizeof(INT_constants[0]);
    lamb.printf("%s defining %d constants\n", me, Nconst);
    for (int i=0; i<Nconst; i++) {
      auto p = INT_constants[i];
      Sexpr_t sym = lamb.mk_symbol(p.name, NIL);
      Sexpr_t val = lamb.mk_integer(p.val, sym);
      lamb.bind_bang(lamb.r5_interaction_environment(), sym, val, NIL);
    }
    
    static const struct { Lamb::Mop3st_t func;  const char *name; } platformfuncs[] = {
      { mop3_lamb_reboot, "lamb-reboot" },
      { mop3_lamb_platform_name, "lamb-platform-name" },
      { mop3_lamb_free_heap, "lamb-free-heap" },
      { mop3_lamb_free_stack, "lamb-free-stack" },
      { mop3_lamb_random, "lamb-random" }
    };
    const Int_t Nplats = sizeof(platformfuncs)/sizeof(platformfuncs[0]);
    lamb.printf("%s defining %d platform functions\n", me, Nplats);
    Sexpr_t env_exec   = lamb.r5_interaction_environment();
    Sexpr_t env_target = lamb.r5_base_environment();
    for (int i=0; i<Nplats; i++) {
      auto p = platformfuncs[i];
      Sexpr_t sym = lamb.mk_symbol(p.name, env_exec);
      lamb.gc_root_push(sym);
      Sexpr_t val = lamb.mk_Mop3_procst_t(p.func, env_exec);
      lamb.gc_root_pop();
      lamb.bind_bang(env_target, sym, val, env_exec);
    }
    
    return NIL;
  }
  
  ll_catch();
}
