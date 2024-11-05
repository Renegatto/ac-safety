#include <SoftwareSerial.h>

SoftwareSerial * newSS(uint8_t rxPin,uint8_t txPin) {
  return new SoftwareSerial(rxPin,txPin);
}

int readSS(SoftwareSerial * ss) {
  return ss->read();
};

size_t writeSS(SoftwareSerial * ss, uint8_t byte) {
  return ss->write(byte);
};