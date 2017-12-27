
sml-wavefile
============

Reader and writer for RIFF/WAV audio files in Standard ML
---------------------------------------------------------

This small library provides a reader (reader.sml) and writer
(writer.sml) for RIFF/WAV PCM audio files written purely in Standard
ML. The reader supports 8, 16, or 24-bit PCM encodings; the writer
supports only 16-bit.

Written by Chris Cannam, cannam@all-day-breakfast.com. MIT licence,
see the file COPYING for details.


TODO
----

* Tests, using included test files
* Support extended IEEE float format
* Write formats other than 16-bit
* Seeking

