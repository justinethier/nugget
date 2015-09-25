#ifndef GC_HEAP_H
#define GC_HEAP_H

/*
notes:
per DLG-successor paper, could have a bin tree (??) of free blocks. tree starts out with a single node of one free block
*/

const int pagesize = 1024; //4096 bytes?
typedef struct {
} heap_header_type;

// TODO: lines within pages


// TODO: alloc
// > pagesize-pageheadersize ==> need multiple pages
// else, find a free line big enough within a page
#endif

