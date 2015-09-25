#ifndef GC_HEAP_H
#define GC_HEAP_H

const int pagesize = 1024; //4096 bytes?
typedef struct {
} heap_header_type;

// TODO: lines within pages


// TODO: alloc
// > pagesize-pageheadersize ==> need multiple pages
// else, find a free line big enough within a page
#endif

