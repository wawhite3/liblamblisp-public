#include "ll_lamb.h"

//!Allocate Lambs dynamically, not statically.  The constructors may depend on Serial.
Lamb *lamb = 0;

void setup()
{
  ME("::setup()");
  unsigned long t_start = millis();
  
  Serial.setTxBufferSize(8192);
  Serial.setRxBufferSize(8192);
  Serial.begin(115200);
  Serial.printf("%s first light @%lu ms\n", me, t_start);
  lamb = new Lamb;
  lamb->setup();
}

void loop()
{
  ME("::loop()");
  ll_try {
    lamb->loop();
  }
  catch (Sexpr_t err) {
    Serial.printf("%s %s\n", me, err->str().c_str());
  }
}
