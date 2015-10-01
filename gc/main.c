// TODO: a basic mark-sweep GC


// HEAP definitions
// experimenting with a heap based off of the one in Chibi scheme
#define gc_free_chunk_size (sizeof(gc_free_list))
typedef struct {
  uint size;
  gc_free_list *next;
} gc_free_list;

typedef struct {
  uint size, chunk_size
  gc_free_list *free_list; // TBD
  gc_heap_block *next; // TBD, linked list is not very efficient, but easy to work with as a start
  char *data;
} gc_heap_block;

gc_heap_block *gc_heap_create(size_t size, size_t chunk_size)
{
  gc_free_list *free, *next;
  gc_heap_block *h;

  // TODO: mmap?
  h = malloc(size);
  if (!h) return NULL;

  h->size = size;
  h->chunk_size = chunk_size;
  h->data = (char *)&(h->data); 
  h->next = NULL;
  free = h->free_list = (gc_free_list *)h->data;
  next = (gc_free_list *)(((char *) free) + gc_free_chunk_size);
  free->size = 0;
  free->next = 
  // TODO: free, next

  return h;
}

void gc_init()
{
}
// END heap definitions

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
