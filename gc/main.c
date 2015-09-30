// TODO: a basic mark-sweep GC

typedef struct {
  unsigned char mark; // mark bits (only need 2)
  // TODO: forwarding address (probably not needed for mark/sweep), anything else???
} gc_header_type;

///// subset of types we'll care about initially:
//#include "types.h"
typedef void *object;
typedef long tag_type;
#define cons_tag 0
#define integer_tag 9
typedef struct {tag_type tag; object cons_car,cons_cdr;} cons_type;
typedef cons_type *list;
typedef struct {tag_type tag; int value;} integer_type;
/////

int colorWhite = 0;
int colorGray  = 1;
int colorBlack = 2;
int colorBlue  = 3;

typedef enum {STATUS_ASYNC, STATUS_SYNC1, STATUS_SYNC2} status_type;

// DLG globals
static void *swept;
static int dirty;
static void *scanned;

// TODO: mutator actions
// TODO: collector
// TODO: extentions
// TODO: proofs, etc
// TODO: revist design using content from kolodner
int main(int argc, char **argv) {
  return 0;
}
