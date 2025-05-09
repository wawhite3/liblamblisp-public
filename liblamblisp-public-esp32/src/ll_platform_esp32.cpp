#include "ll_lamb.h"
#include "ll_platform.h"

#ifdef LAMB_PLATFORM_ESP32

LambPlatform *lambPlatform;

LambPlatform::LambPlatform()	{ lambPlatform = this;  identification();  loop_start_ms = millis();  loop_start_us = micros(); }
LambPlatform::~LambPlatform()	{}

void LambPlatform::loop(void)
{
  loop_start_ms = millis();
  loop_start_us = micros();
}

const char *LambPlatform::name()		{ return "ESP32"; }
Int_t LambPlatform::free_heap()			{ return (Int_t) esp_get_free_heap_size(); }
Int_t LambPlatform::free_stack()		{ return (Int_t) uxTaskGetStackHighWaterMark(NULL); }
void  LambPlatform::rand(byte *buf, Int_t len)	{ esp_fill_random(buf, (size_t) len); }
void  LambPlatform::reboot()			{ esp_restart(); }
Int_t LambPlatform::loop_elapsed_ms()		{ return millis() - loop_start_ms; }
Int_t LambPlatform::loop_elapsed_us()		{ return micros() - loop_start_us; }

void LambPlatform::identification(void)
{
  ME("LambPlatform::identification()");
  
  const char *chip_names[] = { "NONE0", "ESP32", "ESP32_S2", "NONE3", "NONE4", "ESP32-C3","ESP32-H2", "NONE7", "NONE8", "ESP32-S3", "NONE10" };
  const unsigned Nchips    = sizeof(chip_names)/sizeof(chip_names[0]);

  esp_chip_info_t chip_info;
  esp_chip_info(&chip_info);
  if (chip_info.model >= Nchips) chip_info.model = (esp_chip_model_t) 0;
  
  bool hasFlash = chip_info.features & CHIP_FEATURE_EMB_FLASH; 
  uint32_t flash_size = 0;

  if (esp_flash_get_size(NULL, &flash_size) != ESP_OK) global_printf("%s Get flash size failed", me);

  global_printf("%s IDF_target : %s, chip_model : %d, model_name %s, rev %d, full_rev %d\n",
		me, CONFIG_IDF_TARGET, chip_info.model, chip_names[((unsigned) chip_info.model) % Nchips], chip_info.revision, chip_info.full_revision);
  global_printf("%s cores : %d, embedded flash : %lu, features 0x%08x\n",
		me, chip_info.cores, (unsigned long) flash_size, chip_info.features);

#define yesno(_tf_) ((_tf_) ? "yes" : "no")
  global_printf("%s EMB_FLASH : %s, EMB_PSRAM : %s, 80211 : %s, 802154 : %s, BT : %s, BLE : %s\n",
	me,
	yesno(chip_info.features & CHIP_FEATURE_EMB_FLASH),
	yesno(chip_info.features & CHIP_FEATURE_EMB_PSRAM),
	yesno(chip_info.features & CHIP_FEATURE_WIFI_BGN),
	yesno(chip_info.features & CHIP_FEATURE_IEEE802154),
	yesno(chip_info.features & CHIP_FEATURE_BT),
	yesno(chip_info.features & CHIP_FEATURE_BLE)
	);  
#undef yesno
  heap_caps_malloc_extmem_enable(0x400); //1k, why?
  
  global_printf("Heap free:       %d / %d\n",  ESP.getFreeHeap(), ESP.getHeapSize());
  global_printf("Heap max alloc:  %d\n",       ESP.getMaxAllocHeap());
  global_printf("Heap min free:   %d\n",       ESP.getMinFreeHeap());
  global_printf("Chip model       %s rev %d\n",ESP.getChipModel(), ESP.getChipRevision());
  global_printf("PSRAM free:      %d / %d\n",  ESP.getFreePsram(), ESP.getPsramSize());
  global_printf("PSRAM max alloc: %d\n",       ESP.getMaxAllocPsram());
  global_printf("PSRAM min free:  %d\n",       ESP.getMinFreePsram());
  global_printf("Flash size:      %dK\n",      ESP.getFlashChipSize() / 1024);
  global_printf("Flash speed:     %u\n",       ESP.getFlashChipSpeed());
  global_printf("Flash mode:      %d\n",       ESP.getFlashChipMode());
  global_printf("Chip revision:   %d\n",       ESP.getChipRevision());
  global_printf("Chip model:      %s\n",       ESP.getChipModel());
  global_printf("Chip cores:      %d\n",       ESP.getChipCores());
  global_printf("Chip freq:       %u Mhz\n",   ESP.getCpuFreqMHz());
  global_printf("Cycle count:     %u\n",       ESP.getCycleCount());
  global_printf("SDK version:     %s\n",       ESP.getSdkVersion());
  global_printf("Sketch size:     <skipped>\n" /*%u, ESP.getSketchSize()*/);
  global_printf("Sketch MD5:      <skipped>\n" /*%s, ESP.getSketchMD5().c_str()*/);	//takes too long
  global_printf("Sketch free:     %u\n",       ESP.getFreeSketchSpace());
  //DEBT review above for completeness
}

#endif
