// TODO: a basic mark-sweep GC

typedef struct {
  unsigned char mark; // mark bits (only need 2)
  // TODO: forwarding address (probably not needed for mark/sweep), anything else???
} gc_header_type;
#include "types.h"

int main(int argc, char **argv) {
  return 0;
}
