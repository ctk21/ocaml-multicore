#define CAML_INTERNALS

#include "caml/domain.h"
#include "caml/lockfree.h"
#include <stdint.h>

/* Simple Treiber queue for now (a LIFO queue to match existing linked list semantics) */

struct queue_item {
  atomic_intptr_t next;
};

void lockfree_queue_init(struct lockfree_queue* queue) {
    queue->head = NULL;
}

void lockfree_queue_push(struct lockfree_queue* queue, void* queue_item) {
    struct queue_item* new_item = (struct queue_item*)queue_item;

    intptr_t current_head;
    do {
        current_head = atomic_load(&queue->head);

        atomic_store(&new_item->next, (intptr_t)current_head);
    } while(!atomic_compare_exchange_strong(&queue->head, (intptr_t*)&current_head, (intptr_t)new_item));
}

void* lockfree_queue_pop(struct lockfree_queue* queue) {
    intptr_t current_head;
    intptr_t new_head;

    do {
        current_head = atomic_load(&queue->head);

        if( current_head == 0 ) {
            return 0;
        }
        
        new_head = atomic_load(&(((struct queue_item*)current_head)->next));
    } while(!atomic_compare_exchange_strong(&queue->head, (intptr_t*)&current_head, (intptr_t)new_head));

    return (void*)current_head;
}
