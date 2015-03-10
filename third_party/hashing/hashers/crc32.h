// //////////////////////////////////////////////////////////
// crc32.h
// Copyright (c) 2014 Stephan Brumme. All rights reserved.
// see http://create.stephan-brumme.com/disclaimer.html
//

#pragma once

#include "../hashing.h"

/// compute CRC32 hash, based on Intel's Slicing-by-8 algorithm
/** Usage:
    CRC32 crc32;
    std::string myHash  = crc32("Hello World");     // std::string
    std::string myHash2 = crc32("How are you", 11); // arbitrary data, 11 bytes

    // or in a streaming fashion:

    CRC32 crc32;
    while (more data available)
      crc32.add(pointer to fresh data, number of new bytes);
    std::string myHash3 = crc32.getHash();
  */
class CRC32 //: public Hash
{
public:
  /// same as reset()
  CRC32();

  /// compute CRC32 of a memory block
  const char* operator()(const void* data, size_t numBytes);
  /// compute CRC32 of a string, excluding final zero
  const char* operator()(const char* text, size_t size);

  /// add arbitrary number of bytes
  void add(const void* data, size_t numBytes);

  /// return latest hash as 16 hex characters
  const char* getHash();

  /// restart
  void reset();

private:
  /// hash
  uint32_t m_hash;
};
