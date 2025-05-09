
class LambPlatform {
 public:

  //! @name Interaction with the underlying runtime platform.
  //!@{
  LambPlatform();
  ~LambPlatform();
  
  void loop(void);			//!<Perform any platform-specific activity needed at loop() time.  Call only once at beginning main loop().
  void reboot(void);
  
  const char *name();			//!<Return a pointer to a chacter array containing a description of the runtime platform.
  void identification(void);		//!<Emit a string with the complete detailed description of the platform.

  Int_t free_heap();			//!<Return the unused space available for LambLisp expansion.  Whether this is *total* space or *largest* space is platform-dependent.
  Int_t free_stack();			//!<Return the unused stack space available for recursive expansion.  This is useful during debugging to detect runaway stack overflow before it causes a crash.

  void rand(byte *buf, Int_t len);	//!<Fill a buffer with the highest-quality random numbers available on this platform.

  Int_t loop_elapsed_ms();			//!<Return the time elapsed since the beginning of the current loop().
  Int_t loop_elapsed_us();			//!<Return the time elapsed since the beginning of the current loop().
  //!@}

 private:
  Int_t loop_start_ms;
  Int_t loop_start_us;
};

extern LambPlatform *lambPlatform;

#define LAMB_PLATFORM_ESP32 "LAMB_PLATFORM_ESP32"
//define other platforms, but only one at a time
