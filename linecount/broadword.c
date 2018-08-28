#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <immintrin.h>

#define BUFFER_SIZE 8192

size_t process_data(char *text, size_t bytes_read)
{
  size_t popCount = 0;
  char *mask = "\n\n\n\n\n\n\n\n";
  uint64_t wm = *(uint64_t *)mask;

  for (size_t i = 0; i < bytes_read; i += 8) {
    if (i + 8 <= bytes_read) {
      uint64_t w0 = ~(*(uint64_t *)(text + i) ^ wm);
      uint64_t w1 = (w0 >> 4) & w0;
      uint64_t w2 = (w1 >> 2) & w1;
      uint64_t w3 = (w2 >> 1) & w2;

      uint64_t bits = (uint64_t)_pext_u64(w3, 0x0101010101010101L);

      popCount += _popcnt64(bits);
    } else {
      for (size_t j = i; j < bytes_read; ++j) {
        if (text[j] == '\n') {
          ++popCount;
        }
      }
    }
  }

  return popCount;
}

int main(int argc, char **argv)
{
  if (argc < 2) {
    return 1;
  }

  char *in_filename = argv[1];

  FILE *in_file = fopen(in_filename,  "r");

  if (!in_file) {
    return 1;
  }

  char buffer[BUFFER_SIZE];

  size_t total_bytes_read = 0;

  size_t bytes_read = fread(buffer, 1, BUFFER_SIZE, in_file);

  size_t newline_count = 0;

  while (bytes_read > 0) {
    total_bytes_read += bytes_read;

    newline_count += process_data(buffer, bytes_read);

    bytes_read = fread(buffer, 1, BUFFER_SIZE, in_file);
  }

  printf("%zu\n", newline_count);

  fclose(in_file);

  return 0;
}
