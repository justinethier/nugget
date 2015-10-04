/* TODO: a basic mark-sweep GC
   As of now, the GC code is based off the implementation from chibi scheme

 Goals of this project:
 - write algorithms
 - add test cases
 - integrate with types
 - integrate with cyclone
 - extend to tri-color marking an on-the-fly collection
 - etc...
 */
#include <stdlib.h>
#include <stdio.h>

// Types, eventually this has to be integrated with header file
//#include "types.h"
typedef void *object;
typedef long tag_type;
#define cons_tag 0
#define integer_tag 9

#define type_of(x) (((list) x)->tag)
#define is_value_type(x) 0
#define is_object_type(x) (x && !is_value_type(x))
#define is_marked(x) (is_object_type(x) && ((list)x)->hdr.mark)

typedef struct gc_header_type_t gc_header_type;
struct gc_header_type_t {
  unsigned char mark; // mark bits (only need 2)
  // TODO: forwarding address (probably not needed for mark/sweep), anything else???
};

typedef struct {gc_header_type hdr; tag_type tag; object cons_car,cons_cdr;} cons_type;
typedef cons_type *list;
typedef struct {gc_header_type hdr; tag_type tag; int value;} integer_type;
#define car(x) (((list) x)->cons_car)
#define cdr(x) (((list) x)->cons_cdr)

// HEAP definitions
// experimenting with a heap based off of the one in Chibi scheme
#define gc_heap_first_block(h) ((object)(h->data + gc_heap_align(gc_free_chunk_size)))
#define gc_heap_last_block(h) ((object)((char*)h->data + h->size - gc_heap_align(gc_free_chunk_size)))
#define gc_heap_end(h) ((object)((char*)h->data + h->size))
#define gc_free_chunk_size (sizeof(gc_free_list))

#define gc_align(n, bits) (((n)+(1<<(bits))-1)&(((unsigned int)-1)-((1<<(bits))-1)))
// 64-bit is 3, 32-bit is 2
#define gc_word_align(n) gc_align((n), 2)
#define gc_heap_align(n) gc_align(n, 5)

typedef struct gc_free_list_t gc_free_list;
struct gc_free_list_t {
  unsigned int size;
  gc_free_list *next;
};

typedef struct gc_heap_t gc_heap;
struct gc_heap_t {
  unsigned int size;
  unsigned int chunk_size; // 0 for any size, other and heap will only alloc chunks of that size
  gc_free_list *free_list; // TBD
  gc_heap *next; // TBD, linked list is not very efficient, but easy to work with as a start
  char *data;
};

gc_heap *gc_heap_create(size_t size, size_t chunk_size)
{
  gc_free_list *free, *next;
  gc_heap *h;
  // TODO: mmap?
  h = malloc(size);
  if (!h) return NULL;
  h->size = size;
  h->chunk_size = chunk_size;
printf("DEBUG h->data addr: %p\n", &(h->data));
  h->data = (char *) gc_heap_align(sizeof(h->data) + (uint)&(h->data)); 
printf("DEBUG h->data addr: %p\n", h->data);
  h->next = NULL;
  free = h->free_list = (gc_free_list *)h->data;
  next = (gc_free_list *)(((char *) free) + gc_heap_align(gc_free_chunk_size));
  free->size = 0; // First one is just a dummy record
  free->next = next;
  next->size = size - gc_heap_align(gc_free_chunk_size);
  next->next = NULL;
  return h;
}

void *gc_try_alloc(gc_heap *h, size_t size) 
{
  gc_free_list *f1, *f2, *f3;
  for (; h; h = h->next) { // All heaps
    // TODO: chunk size (ignoring for now)

    for (f1 = h->free_list, f2 = f1->next; f2; f1 = f2, f2 = f2->next) { // all free in this heap
      if (f2->size > size) { // Big enough for request
        // TODO: take whole chunk or divide up f2 (using f3)?
        if (f2->size >= (size + gc_heap_align(1) /* min obj size */)) {
          f3 = (gc_free_list *) (((char *)f2) + size);
          f3->size = f2->size - size;
          f3->next = f2->next;
          f1->next = f3;
        } else { /* Take the whole chunk */
          f1->next = f2->next;
        }
        return f2;
      }
    }
  }
  return NULL; 
}

void *gc_alloc(gc_heap *h, size_t size) 
{
  // TODO: check return value, if null (could not alloc) then 
  // run a collection and check how much free space there is. if less
  // the allowed ratio, try growing heap.
  // then try realloc. if cannot alloc now, then throw out of memory error
  size = gc_heap_align(size);
  return gc_try_alloc(h, size);
}

size_t gc_allocated_bytes(object obj)
{
  tag_type t;
  if (is_value_type(obj))
    return gc_heap_align(1);
  t = type_of(obj); 
  if (t == cons_tag) return sizeof(cons_type);
  if (t == integer_tag) return sizeof(integer_type);
  
  fprintf(stderr, "cannot get size of object %ld\n", t);
  return 0;
}

size_t gc_heap_total_size(gc_heap *h)
{
  size_t total_size = 0;
  while(h) {
    total_size += h->size;
    h = h->next;
  }
  return total_size;
}

void gc_mark(gc_heap *h, object obj)
{
  if (!obj || is_marked(obj))
    return;

 ((list)obj)->hdr.mark = 1;
 // TODO: mark heap saves (??) 
 // could this be a write barrier?
 
 // Mark objects this one references
 if (type_of(obj) == cons_tag) {
   gc_mark(h, car(obj)); 
   gc_mark(h, cdr(obj)); 
 }
 // TODO: will be more work in here the "real" implementation
}

void gc_sweep(gc_heap *h, size_t *sum_freed_ptr)
{
  // TODO: scan entire heap, freeing objects that have not been marked
  size_t freed, max_freed=0, sum_freed=0, size;
  object p, end;
  gc_free_list *q, *r, *s;
/*
  for (; h; h = h->next) { // All heaps
    p = gc_heap_first_block(h);
    q = h->free_list;
    end = gc_heap_end(h);
    while (p < end) {
      // find preceding/succeeding free list pointers
      for (r = q->next; r && ((char *)r < (char *)p); q=r, r=r->next);

      if ((char *)r == (char *)p) { // this is a free block, skip it
        p = (object) (((char *)p) + r->size);
        continue;
      }
      size = gc_heap_align(gc_allocated_bytes(p));
      if (!is_marked(p)) {
        // TODO: free p
      } else {
        ((list)p)->mark = 0;
        p = (object)(((char *)p) + size);
      }
    }
  }

*/
  // FUTURE: return amount freed via int ptr AND scheme obj
  if (sum_freed_ptr) *sum_freed_ptr = sum_freed;
  return;
}

void gc_collect(gc_heap *h, size_t *sum_freed) 
{
  printf("(heap: %p size: %d)", h, (unsigned int)gc_heap_total_size(h));
  // TODO: mark globals
  // TODO: gc_mark(h, h);
  // conservative mark?
  // weak refs?
  // finalize?
  gc_sweep(h, sum_freed);
  // debug print free stats
  // return value from sweep??
}

// void gc_init()
// {
// }
// END heap definitions

/* tri-color GC stuff, we will care about this later...
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
*/

int main(int argc, char **argv) {
  gc_heap *h = gc_heap_create(8 * 1024 * 1024, 0);
  void *obj1 = gc_alloc(h, 1);
  void *obj2 = gc_alloc(h, 3);
  void *obj3 = gc_alloc(h, 1);

  // Build up an object graph to test collection...
  return 0;
}
