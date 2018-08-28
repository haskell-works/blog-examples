#include <stdio.h>
#include <fcntl.h>
#include <mmintrin.h>
#include <unistd.h>
#include <immintrin.h>
#include <string.h>

#define BUFFER_SIZE 8192

// __m256i _mm256_broadcastb_epi8 (__m128i a)
// __m256i _mm256_cmpeq_epi64 (__m256i a, __m256i b)

size_t process_data(char *text, size_t bytes_read, __m256i ws_newlines)
{
  char *bytes_end = text + bytes_read;
  size_t popCount = 0;

  if ((bytes_read & 0x1f) == 0) {
    for (; text != bytes_end; text += 32) {
      __m256i matches_bytes = _mm256_cmpeq_epi8(*(__m256i*)text, ws_newlines);

      int matches_bits = _mm256_movemask_epi8(matches_bytes);

      popCount += _mm_popcnt_u32((uint32_t)matches_bits);
    }
  } else {
    for (size_t i = 0; i < bytes_read; i += 32) {
      size_t bytes_read_iter = bytes_read - i;

      if (bytes_read_iter > 32) {
        bytes_read_iter = 32;
      }

      __m256i matches_bytes = _mm256_cmpeq_epi8(*(__m256i*)(text + i), ws_newlines);

      int matches_bits = _mm256_movemask_epi8(matches_bytes);

      popCount += _mm_popcnt_u32((uint32_t)((0xffffffff >> (32 - bytes_read_iter)) & (uint32_t)matches_bits));
    }
  }

  return popCount;
}

__m256i broadcast_u8(uint8_t w)
{
  __m128i w128 = {w};

  return _mm256_broadcastb_epi8(w128);
}

int main(int argc, char **argv)
{
  if (argc < 2) {
    return 1;
  }

  char *in_filename  = argv[1];

  FILE *in_file   = fopen(in_filename,  "r");

  if (!in_file) {
    return 1;
  }

  char buffer[BUFFER_SIZE];

  __m256i ws_newline = broadcast_u8((uint8_t)'\n');

  size_t total_bytes_read = 0;

  size_t bytes_read = fread(buffer, 1, BUFFER_SIZE, in_file);

  size_t newline_count = 0;

  while (bytes_read > 0) {
    total_bytes_read += bytes_read;

    newline_count += process_data(buffer, bytes_read, ws_newline);

    bytes_read = fread(buffer, 1, BUFFER_SIZE, in_file);
  }

  printf("%zu\n", newline_count);

  fclose(in_file);

  return 0;
}
