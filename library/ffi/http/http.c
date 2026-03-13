#include "http.h"

#include <openssl/evp.h>
#include <openssl/sha.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

int32_t kaede_http_ws_accept(const char *key, size_t key_len, char *out,
                             size_t out_len) {
  static const char guid[] = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
  const size_t guid_len = sizeof(guid) - 1U;
  unsigned char digest[SHA_DIGEST_LENGTH];
  unsigned char encoded[29];
  unsigned char *input;
  size_t input_len;
  int encoded_len;

  if (key == NULL || out == NULL || key_len == 0U || out_len < 28U) {
    return -1;
  }

  if (key_len > SIZE_MAX - guid_len) {
    return -1;
  }

  input_len = key_len + guid_len;
  input = malloc(input_len);
  if (input == NULL) {
    return -1;
  }

  memcpy(input, key, key_len);
  memcpy(input + key_len, guid, guid_len);

  if (SHA1(input, input_len, digest) == NULL) {
    free(input);
    return -1;
  }

  free(input);

  encoded_len = EVP_EncodeBlock(encoded, digest, SHA_DIGEST_LENGTH);
  if (encoded_len != 28) {
    return -1;
  }

  memcpy(out, encoded, 28U);
  return 28;
}
