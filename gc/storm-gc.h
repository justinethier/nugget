#ifndef __GC_HEADER__
#define __GC_HEADER__

typedef struct gc_header_type_t gc_header_type;
struct gc_header_type_t {
  unsigned char mark; // mark bits (only need 2)
  // TODO: forwarding address (probably not needed for mark/sweep), anything else???
};

typedef struct gc_free_list_t gc_free_list;
struct gc_free_list_t {
  unsigned int size;
  gc_free_list *next;
};

typedef struct gc_heap_t gc_heap;
struct gc_heap_t {
  unsigned int size;
  unsigned int chunk_size; // 0 for any size, other and heap will only alloc chunks of that size
  unsigned int max_size;
  gc_free_list *free_list; // TBD
  gc_heap *next; // TBD, linked list is not very efficient, but easy to work with as a start
  char *data;
};

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

gc_heap *gc_heap_create(size_t size, size_t max_size, size_t chunk_size);
int gc_grow_heap(gc_heap *h, size_t size, size_t chunk_size);
void *gc_try_alloc(gc_heap *h, size_t size);
void *gc_alloc(gc_heap *h, size_t size);
size_t gc_allocated_bytes(object obj);
gc_heap *gc_heap_last(gc_heap *h);
size_t gc_heap_total_size(gc_heap *h);
void gc_mark(gc_heap *h, object obj);
size_t gc_sweep(gc_heap *h, size_t *sum_freed_ptr);
//void gc_collect(gc_heap *h, size_t *sum_freed) 

#endif
