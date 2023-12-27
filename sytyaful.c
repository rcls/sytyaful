#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#undef _NDEBUG
#include <assert.h>

#define MAX_ALIGN (_Alignof(max_align_t))

typedef struct Binding Binding;
typedef struct Arena Arena;

typedef struct Arena {
    struct {
        struct Arena * chain;
        size_t total;
        size_t used;
    };
    _Alignas(max_align_t) uint8_t storage[];
} Arena;

#define ARENA_BASE (offsetof(Arena, storage))

static void * arena_alloc(Arena ** arena, size_t size)
{
    // Round up to a multiple of maximum alignment.
    size = (size + MAX_ALIGN - 1) & -MAX_ALIGN;
    if (*arena != NULL && size <= (*arena)->total - (*arena)->used) {
        // Allocate the space...
        void * result = (*arena)->storage + (*arena)->used;
        (*arena)->used += size;
        return result;
    }
    size_t extend = *arena ? 2 * (*arena)->total : 480;
    size_t total = size > extend ? size : extend;
    Arena * a = malloc(offsetof(Arena, storage) + total);
    assert(a);
    a->chain = *arena;
    *arena = a;
    a->total = total;
    a->used = (size + MAX_ALIGN - 1) & -MAX_ALIGN;
    return a->storage;
}

static void arena_free(Arena ** arena_pointer)
{
    /* if (true) */
    /*     return; */
    uint64_t total = 0;
    Arena * arena = *arena_pointer;
    while (arena != NULL) {
        total += arena->total;
        Arena * a = arena->chain;
        free(arena);
        arena = a;
    }
    //printf("Freed %lu\n", total);
    *arena_pointer = NULL;
}

typedef struct Predicate Predicate;
typedef bool PredicateF(Predicate * binding, uint64_t n);

struct Predicate {
    PredicateF * function;
    // ... actual users follow with more data.
};

typedef struct Measure Measure;
typedef bool MeasureF(Measure * binding, Arena ** a, Predicate * p);
struct Measure {
    MeasureF * function;
};

typedef struct BoundSearch1 {
    Predicate b;
    Predicate * x;
    Arena ** arena;
    uint64_t low;
    uint64_t pivot;
    uint64_t high;
    Measure * q;
} BoundSearch1;

typedef struct BoundSearch2 {
    // FIXME - if we always kept the length, then laziness could be done
    // via copying in the result.
    Predicate b;
    Arena ** arena;
    Predicate * y;
    Predicate * x;
    uint64_t high;
    uint64_t pivot;
    Measure * q;
} BoundSearch2;

typedef struct BoundMeasure1 {
    // TODO - it seems every measure is created in the context of a predicate,
    // so we could do some combining...
    Measure m;
    BoundSearch1 * s;
} BoundMeasure1;


typedef struct BoundMeasure2 {
    // TODO - it seems every measure is created in the context of a predicate,
    // so we could do some combining...
    Measure m;
    BoundSearch2 * s;
    Predicate * x;
} BoundMeasure2;

typedef struct BoundMerge {
    Predicate b;
    uint64_t pivot;
    Predicate * x;
    Predicate * y;
} BoundMerge;

static Predicate * range(
    Arena ** a, uint64_t low, uint64_t high, Measure * q);

static bool merge_worker(Predicate * p, uint64_t n)
{
    BoundMerge * m = (BoundMerge *) p;
    if (n < m->pivot)
        return m->x->function(m->x, n);
    else
        return m->y->function(m->y, n);
}

static Predicate * merge(
    Arena ** a, uint64_t pivot, Predicate * x, Predicate * y)
{
    BoundMerge * m = arena_alloc(a, sizeof(BoundMerge));
    m->b.function = merge_worker;
    m->pivot = pivot;
    m->x = x;
    m->y = y;
    return (Predicate *) m;
}

static bool search2measure(Measure * me, Arena ** a, Predicate * y)
{
    BoundSearch2 * s = ((BoundMeasure2 *) me)->s;
    Predicate * merged = merge(a, s->pivot, s->x, y);
    return s->q->function(s->q, a, merged);
}

static bool search2worker(Predicate * p, uint64_t n)
{
    BoundSearch2 * s = (BoundSearch2 *) p;
    if (!s->y) {
        BoundMeasure2 * m = arena_alloc(s->arena, sizeof(BoundMeasure2));
        m->m.function = search2measure;
        m->s = s;
        s->y = range(s->arena, s->pivot, s->high, (Measure *) m);
    }

    return s->y->function(s->y, n);
}


static Predicate * search_high(
    Arena ** a, uint64_t pivot, Predicate * x, uint64_t high, Measure * q)
{
    BoundSearch2 * y = arena_alloc(a, sizeof(BoundSearch2));
    y->b.function = search2worker;
    y->y = NULL;
    y->arena = a;
    y->x = x;
    y->pivot = pivot;
    y->high = high;
    y->q = q;
    return (Predicate *) y;
}

static bool search1measure(Measure * me, Arena ** a, Predicate * x)
{
    BoundSearch1 * s = ((BoundMeasure1 *) me)->s;
    Predicate * merged = merge(
        a, s->pivot, x, search_high(a, s->pivot, x, s->high, s->q));
    return s->q->function(s->q, a, merged);
}

static bool search1worker(Predicate * p, uint64_t n)
{
    BoundSearch1 * s = (BoundSearch1 *) p;
    if (!s->x) {
        BoundMeasure1 * m = arena_alloc(s->arena, sizeof(BoundMeasure1));
        m->m.function = search1measure;
        m->s = s;
        s->x = range(s->arena, s->low, s->pivot, (Measure *) m);
    }

    return s->x->function(s->x, n);
}

static Predicate * split(
    Arena ** a, uint64_t low, uint64_t pivot, uint64_t high, Measure * q)
{
    BoundSearch1 * x = arena_alloc(a, sizeof(BoundSearch1));
    x->b.function = search1worker;
    x->arena = a;
    x->x = NULL;
    x->low = low;
    x->pivot = pivot;
    x->high = high;
    x->q = q;
    Predicate * y = search_high(a, pivot, (Predicate *) x, high, q);
    return merge(a, pivot, (Predicate *) x, y);
}

static bool returnTrue(Predicate * p, uint64_t n)
{
    return true;
}

static bool returnFalse(Predicate * p, uint64_t n)
{
    return false;
}

static Predicate constTrue = { returnTrue };

static Predicate constFalse = { returnFalse };

// We overload `high==0` to mean unbounded, unlike the versions in other
// languages, which split out that as a separate function `after`.
static Predicate * range(Arena ** a, uint64_t low, uint64_t high, Measure * q)
{
    if (high == low + 1)
        return q->function(q, a, &constTrue) ? &constTrue : &constFalse;

    uint64_t pivot;
    if (high == 0)
        pivot = low * 2 + 1;
    else
        pivot = low + (high - low) / 2;

    return split(a, low, pivot, high, q);
}


static uint64_t limit(Predicate * p)
{
    uint64_t m = 0;
    uint64_t n = 1;
    while (p->function(p, n)) {
        m = n;
        n = 2 * m + 1;
    }
    while (n - m > 1) {
        uint64_t pivot = m + (n - m) / 2;
        if (p->function(p, pivot))
            m = pivot;
        else
            n = pivot;
    }
    return m;
}

typedef struct Raw {
    uint64_t pivot;
    const struct Raw * tt;
    const struct Raw * ff;
} Raw;

static const Raw RawT = { 1, NULL, NULL };
static const Raw RawF = { 0, NULL, NULL };


static bool arbitrary(Predicate * p, uint64_t n)
{
    return n & 1;
}

static struct Predicate Arbitrary = { arbitrary };

typedef struct NegMeasure {
    Measure b;
    Measure * q;
} NegMeasure;

static bool negMeasure(Measure * m, Arena ** a, Predicate * p)
{
    NegMeasure * n = (NegMeasure *) m;
    return !n->q->function(n->q, a, p);
}

typedef struct Partial {
    Predicate b;
    Measure * q;
    Predicate * different;
    bool p_arbitrary;
} Partial;

static bool partialF(Predicate * pr, uint64_t n)
{
    Partial * p = (Partial *) pr;
    Arena * a = NULL;
    Predicate * merged = merge(&a, n, &Arbitrary, p->different);
    bool q_merged = p->q->function(p->q, &a, merged);
    arena_free(&a);
    return q_merged != p->p_arbitrary;
}

typedef struct Special {
    Predicate b;
    Predicate * inner;
    uint64_t n;
    bool v;
} Special;

bool special_worker(Predicate * p, uint64_t n)
{
    Special * s = (Special *) p;
    if (n == s->n)
        return s->v;
    else
        return s->inner->function(s->inner, n);
}

static Predicate * special(Arena ** a, Predicate * p, uint64_t n, bool v)
{
    Special * s = arena_alloc(a, sizeof(Special));
    s->b.function = special_worker;
    s->inner = p;
    s->n = n;
    s->v = v;
    return (Predicate *) s;
}

typedef struct Specialize {
    Measure b;
    Measure * inner;
    uint64_t n;
    bool v;
} Specialize;

static bool specialize_worker(Measure * m, Arena ** a, Predicate * p)
{
    Specialize * s = (Specialize *) m;
    Predicate * sp = special(a, p, s->n, s->v);
    return s->inner->function(s->inner, a, sp);
}

static Measure * specialize(Arena ** a, Measure * m, uint64_t n, bool v)
{
    Specialize * s = arena_alloc(a, sizeof(Specialize));
    s->b.function = specialize_worker;
    s->inner = m;
    s->n = n;
    s->v = v;
    return (Measure *) s;
}

static const Raw * raw(Arena ** ar, Measure * q)
{
    Arena * a = NULL;
    bool p_arbitrary = q->function(q, &a, &Arbitrary);
    arena_free(&a);
    struct {
        Measure b;
        Measure * q;
    } negarg = { { negMeasure }, q };
    Predicate * different = range(
        &a, 0, 0, p_arbitrary ? (Measure *) &negarg : q);
    if (q->function(q, &a, different) == p_arbitrary) {
        arena_free(&a);
        return p_arbitrary ? &RawT : &RawF;
    }
    Partial partial = {
        { partialF }, q, different, p_arbitrary };
    uint64_t pivot = limit((Predicate *) &partial);
    arena_free(&a);

    Raw * node = arena_alloc(ar, sizeof(Raw));
    node->pivot = pivot;

    node->tt = raw(ar, specialize(&a, q, pivot, true));
    arena_free(&a);

    node->ff = raw(ar, specialize(&a, q, pivot, false));
    arena_free(&a);

    return node;
}

static bool raw_eq(const Raw * l, const Raw * r)
{
    if (l == r)
        return true;
    if (l->tt == NULL || r->tt == NULL)
        return false;
    return l->pivot == r->pivot && raw_eq(l->tt, r->tt) && raw_eq(l->ff, r->ff);
}

// Check if l is the same as a slicing of r.
static bool raw_eq_slice(const Raw * l, const Raw * r, uint64_t pivot, bool v)
{
    if (r->tt && r->pivot == pivot)
        return raw_eq(l, v ? r->tt : r->ff);

    if (l == r)
        return true;

    if (r->tt == NULL)
        return false;

    // If the top level pivots match, then unwrap and recurse.
    if (l->tt && l->pivot == r->pivot)
        return raw_eq_slice(l->tt, r->tt, pivot, v)
            && raw_eq_slice(l->ff, r->ff, pivot, v);
    else
        // Otherwise, we can match if both branches of r match.
        return raw_eq_slice(l, r->tt, pivot, v)
            && raw_eq_slice(l, r->ff, pivot, v);
}

// Move to new arena...
static const Raw * dup(Arena ** a, const Raw * r)
{
    if (r->tt == NULL)
        return r;
    Raw * node = arena_alloc(a, sizeof(Raw));
    node->pivot = r->pivot;
    node->tt = dup(a, r->tt);
    node->ff = dup(a, r->ff);
    return node;
}

// Always copy....
static const Raw * slice(Arena ** a, const Raw * r, uint64_t pivot, bool v)
{
    if (r->tt == NULL)
        return r;
    if (r->pivot == pivot)
        return dup(a, v ? r->tt : r->ff);
    const Raw * tt = slice(a, r->tt, pivot, v);
    if (raw_eq_slice(tt, r->ff, pivot, v))
        return tt;
    const Raw * ff = slice(a, r->ff, pivot, v);
    /* if (raw_eq(tt, ff)) */
    /*     return tt; */
    Raw * node = arena_alloc(a, sizeof(Raw));
    node->pivot = r->pivot;
    node->tt = tt;
    node->ff = ff;
    return node;
}

#define HT_PRIME 31
typedef struct HTNode {
    struct HTNode * next;
    uint64_t n;
    double weight;
} HTNode;
typedef struct HashTable {
    HTNode * buckets[HT_PRIME];
} HashTable;

static void weights(Arena ** a, HashTable * ht, double w, const Raw * r)
{
    if (r->tt == NULL)
        return;
    const double GOLD =  0.61803398875;
    weights(a, ht, w * GOLD, r->tt);
    weights(a, ht, w * GOLD, r->ff);
    HTNode ** b = &ht->buckets[r->pivot % HT_PRIME];
    for (; *b; b = &(*b)->next) {
        if ((*b)->n == r->pivot) {
            (*b)->weight += w;
            return;
        }
    }
    *b = arena_alloc(a, sizeof(HTNode));
    (*b)->next = NULL;
    (*b)->n = r->pivot;
    (*b)->weight = w;
}

static uint64_t max_weight(const Raw * r)
{
    HashTable ht;
    memset(&ht, 0, sizeof(ht));
    Arena * a = NULL;
    weights(&a, &ht, 1, r);
    uint64_t pivot = 0;
    double max_w = 0;
    for (int i = 0; i < HT_PRIME; ++i) {
        for (HTNode * n = ht.buckets[i]; n; n = n->next) {
            if (n->weight > max_w) {
                max_w = n->weight;
                pivot = n->n;
            }
        }
    }
    arena_free(&a);
    return pivot;
}


static const Raw * optimize(Arena ** a, const Raw * r)
{
    if (r->tt == NULL)
        return r;
    uint64_t pivot = max_weight(r);
    Raw * node = arena_alloc(a, sizeof(Raw));
    node->pivot = pivot;
    Arena * temp = NULL;
    node->tt = optimize(a, slice(&temp, r, pivot, true));
    arena_free(&temp);
    node->ff = optimize(a, slice(&temp, r, pivot, false));
    arena_free(&temp);
    return node;
}


static void cook(const Raw * r)
{
    if (r->tt == NULL) {
        putchar(r->pivot ? 'T' : 'F');
        return;
    }
    if (r->tt->tt == NULL && r->ff->tt == NULL) {
        if (r->tt->pivot > r->ff->pivot) {
            printf("@%lu", r->pivot);
            return;
        }
        if (r->tt->pivot < r->ff->pivot) {
            printf("!%lu", r->pivot);
            return;
        }
    }
    if (r->tt->tt == NULL) {
        if (r->tt->pivot)
            printf("@%lu||", r->pivot);
        else
            printf("!%lu&&", r->pivot);
        cook(r->ff);
        return;
    }
    if (r->ff->tt == NULL) {
        if (r->ff->pivot)
            printf("!%lu||", r->pivot);
        else
            printf("@%lu&&", r->pivot);
        cook(r->tt);
        return;
    }
    printf("(IF %lu ", r->pivot);
    cook(r->tt);
    putchar(' ');
    cook(r->ff);
    putchar(')');
}

static bool martin(Measure * m, Arena ** a, Predicate * p)
{
    int n = 0;
    if (p->function(p, 111111111111111))
        n += 1;
    if (p->function(p, 222222222222222))
        n += 2;
    if (p->function(p, 333333333333333))
        n += 4;
    if (p->function(p, 444444444444444))
        n += 8;
    return p->function(p, n) != p->function(p, n+1);
}

static Measure Martin = { martin };

int main(void)
{
    Arena * a = NULL;
    const Raw * r = raw(&a, &Martin);
    Arena * b = NULL;
    r = optimize(&b, r);
    arena_free(&a);
    r = optimize(&a, r);
    arena_free(&b);
    cook(r);
    printf("\n");
    arena_free(&a);
    return 0;
}
