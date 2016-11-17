#ifndef LIST_H
#define LIST_H

// Intrusive list, usage example:
//
// #include <stddef.h>
//
// typedef struct
// {
// 	lnode hdr;			// CAN BE ANYWHERE
// 	...
// }
// 	node;
//
/*
#define LIST_INIT(l) list_init(l)
#define LIST_COUNT(l) list_count(l)
#define LIST_PREV(n) ((node*)(list_prev(&(n)->hdr)-offsetof(node,hdr)))
#define LIST_NEXT(n) ((node*)(list_next(&(n)->hdr)-offsetof(node,hdr)))
#define LIST_FRONT(l) ((node*)(list_front(l)-offsetof(node,hdr)))
#define LIST_BACK(l) (((node*)list_back(l)-offsetof(node,hdr)))
#define LIST_REMOVE(l,n) list_remove(l,&(n)->hdr)
#define LIST_PUSH_FRONT(l,n) list_push_front(l,&(n)->hdr)
#define LIST_PUSH_BACK(l,n) list_push_back(l,&(n)->hdr)
#define LIST_POP_FRONT(l) ((node*)(list_pop_front(l)-offsetof(node,hdr)))
#define LIST_POP_BACK(l) ((node*)(list_pop_back(l)-offsetof(node,hdr)))
#define LIST_INSERT_BEFORE(l,n,v) list_insert_before(l,&(n)->hdr,&(v)->hdr)
#define LIST_INSERT_AFTER(l,n,v) list_insert_after(l,&(n)->hdr,&(v)->hdr)
#define LIST_CONCAT(l1,l2) list_concat(l1,l2)
#define LIST_ITER(l,f,d) list_iter(l,(int (*)(lnode*,void*))f,d)
#define LIST_CLEAR(l) list_clear(l)
*/
//

typedef struct list_ list;

// Define struct so can do static or stack allocation, or malloc
// it to create on the heap. Use 'list_init' then to prepare.

typedef struct lnode_ lnode;
struct lnode_ { lnode *prev, *next; };
struct list_ { lnode *front, *back; size_t cnt; };

#define list_init(l) { (l)->cnt = 0; (l)->front = (l)->back = NULL; }
#define list_count(l) (l)->cnt
#define list_front(l) (l)->front
#define list_back(l) (l)->back
#define list_prev(n) (n)->prev
#define list_next(n) (n)->next

extern void list_clear(list *l);
extern void list_push_front(list *l, lnode *n);
extern void list_push_back(list *l, lnode *n);
extern void list_insert_before(list *l, lnode *n, lnode *v);
extern void list_insert_after(list *l, lnode *n, lnode *v);

extern void list_remove(list *l, const lnode *n);
extern lnode *list_pop_front(list *l);
extern lnode *list_pop_back(list *l);

extern void list_concat(list *l, list *from);	// from is emptied

// Iterate over lnodes while callback returns non-zero
extern void list_iter(list *l, int (*callback)(lnode*,void *p1), void *p1);


#endif
