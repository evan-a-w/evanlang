#include <stdbool.h>
#include <stdlib.h>

#include "garbage_collector.h"

#define HEADER_SIZE (sizeof(struct header))

typedef struct header {
    size_t size;
    // Only used if it's in the large young heap
    struct header *next;
    void (*trace)(void *self);
    void (*finalize)(void *self);
    bool traced;
    void **data;
} header_t;

typedef struct gc_state {
    // young_heap should always have size YOUNG_HEAP_SIZE
    char *young_heap; 
    char *young_heap_next;
    struct header *large_young_heap;

    char *old_heap;
    size_t old_heap_size;
    size_t old_heap_capacity;
} gc_state_t;

static void trace_roots(void);

gc_state_t gc_state;

bool gc_init() {
    gc_state.young_heap = malloc(YOUNG_HEAP_SIZE);
    if (gc_state.young_heap == NULL) {
        return false;
    }
    gc_state.young_heap_next = gc_state.young_heap;
    gc_state.large_young_heap = NULL;

    gc_state.old_heap = NULL;
    gc_state.old_heap_size = 0;
    gc_state.old_heap_capacity = 0;
    
    return true;
}

static bool header_init(header_t *header,
                        size_t size,
                        header_t *next,
                        void (*trace)(void *),
                        void (*finalize)(void *),
                        void *data)
{
    header->size = size;
    header->next = next;
    header->trace = trace;
    header->finalize = finalize;
    header->traced = false;

    header->data = malloc(sizeof(*header->data));
    if (header->data == NULL)
        return false;
    header->data[0] = data;

    return true;
}

static void gc_collect_minor(void) {
    
}

void **galloc(size_t size, void (*trace)(void *), void (*finalize)(void *)) {
    // If size is too big, we just use malloc
    if (size > YOUNG_HEAP_SIZE) {
        header_t *header = malloc(size + HEADER_SIZE);
        if (header == NULL)
            return NULL;
        if (header_init(header, size, gc_state.large_young_heap, trace, finalize,
                        (char *)header + HEADER_SIZE)) {
            free(header);
            return NULL;
        }
        gc_state.large_young_heap = header;

        return header->data;
    }

    // If we're out of space, we need to collect
    if (gc_state.young_heap_next + size > gc_state.young_heap + HEADER_SIZE) {
        gc_collect_minor();
    }

    // Now there should be enough space
    header_t *header = (header_t *)gc_state.young_heap_next;
    if (header_init(header, size, NULL, trace, finalize, (char *)header + HEADER_SIZE))
        return NULL;
    gc_state.young_heap_next += size + HEADER_SIZE;

    return header->data;
}

void gc_destroy() {

}
