#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#undef _NDEBUG
#include <assert.h>

#define MAX_ALIGN (_Alignof(max_align_t))
#define ARENA_BYTES 4000

typedef struct Binding Binding;
typedef struct Arena Arena;

typedef struct Arena {
    struct Arena * chain;
    size_t used;

    _Alignas(max_align_t) uint8_t storage[ARENA_BYTES];
} Arena;
_Static_assert(sizeof(Arena) <= ARENA_BYTES + MAX_ALIGN + 32, "");

static struct Arena * free_arenas;

#define ARENA_BASE (offsetof(Arena, storage))

static void arena_init(Arena * arena)
{
    arena->chain = arena;
    arena->used = 0;
}

static void * arena_alloc(Arena * arena, size_t size)
{
    // Round up to a multiple of maximum alignment.
    size = (size + MAX_ALIGN - 1) & -MAX_ALIGN;
    Arena * current = arena->chain;
    if (size <= ARENA_BYTES - current->used) {
        // Allocate the space...
        void * result = current->storage + current->used;
        current->used += size;
        return result;
    }
    assert(size <= ARENA_BYTES);
    Arena * a;
    if (free_arenas) {
        a = free_arenas;
        free_arenas = a->chain;
    }
    else {
        a = malloc(sizeof(Arena));
        assert(a);
    }
    a->chain = current;
    a->used = size;
    arena->chain = a;
    return a->storage;
}

static void arena_free(Arena * arena)
{
    //uint64_t used = arena->used;
    Arena * tail = arena->chain;
    if (tail != arena) {
        while (tail->chain != arena)
            tail = tail->chain;
        tail->chain = free_arenas;
        free_arenas = arena->chain;
    }
    arena_init(arena);
}

typedef struct Predicate Predicate;
typedef bool PredicateF(Predicate * binding, uint64_t n);

struct Predicate {
    PredicateF * predicate;
    // ... actual users follow with more data.
};

typedef struct BoundMerge {
    Predicate b;
    uint64_t pivot;
    Predicate * x;
    Predicate * y;
} BoundMerge;

// A measure is always contained inside a predicate.
typedef struct Measure Measure;
typedef bool MeasureF(Measure * binding, Arena * a, Predicate * p);
struct Measure {
    BoundMerge merge;
    MeasureF * measure;
};

typedef struct DeferredSearch1 {
    Predicate b;
    Arena * arena;
    uint64_t low;
    uint64_t mid;
    uint64_t high;
    Measure * q;
} DeferredSearch1;

typedef struct DeferredSearch2 {
    Predicate b;
    Arena * arena;
    Predicate * x;
    uint64_t mid;
    uint64_t high;
    Measure * q;
} DeferredSearch2;

typedef struct ResolvedSearch1 {
    Measure m;
    uint64_t mid;
    uint64_t high;
    Measure * q;
} ResolvedSearch1;

typedef struct ResolvedSearch2 {
    Measure m;
    uint64_t mid;
    Predicate * x;
    Measure * q;
} ResolvedSearch2;

#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define BOUND_SEARCH1_BYTES ( \
        MAX(sizeof(DeferredSearch1), sizeof(ResolvedSearch1)))
#define BOUND_SEARCH2_BYTES ( \
        MAX(sizeof(DeferredSearch2), sizeof(ResolvedSearch2)))

static void range(BoundMerge * target,
                  Arena * a, uint64_t low, uint64_t high, Measure * q);

static bool merge_worker(Predicate * p, uint64_t n)
{
    BoundMerge * m = (BoundMerge *) p;
    if (n < m->pivot)
        return m->x->predicate(m->x, n);
    else
        return m->y->predicate(m->y, n);
}

static Predicate * merge(
    Arena * a, uint64_t pivot, Predicate * x, Predicate * y)
{
    BoundMerge * m = arena_alloc(a, sizeof(BoundMerge));
    m->b.predicate = merge_worker;
    m->pivot = pivot;
    m->x = x;
    m->y = y;
    return (Predicate *) m;
}

static bool search2measure(Measure * me, Arena * a, Predicate * y)
{
    ResolvedSearch2 * s = (ResolvedSearch2 *) me;
    Predicate * merged = merge(a, s->mid, s->x, y);
    return s->q->measure(s->q, a, merged);
}

static bool search2worker(Predicate * p, uint64_t n)
{
    // We transmute in place from a lazily deferred value to the computed value.
    // We have space for both.  (The call to range() below produces either
    // a merge or a constant, both of which we have left space for.)
    DeferredSearch2 * s = (DeferredSearch2 *) p;
    Arena * arena = s->arena;
    Predicate * x = s->x;
    uint64_t mid  = s->mid;
    uint64_t high = s->high;
    Measure * q   = s->q;

    ResolvedSearch2 * r = (ResolvedSearch2 *) s;
    r->mid = mid;
    r->x = x;
    r->q = q;
    r->m.measure = search2measure;

    assert((uintptr_t) p == (uintptr_t) &r->m.merge);
    range(&r->m.merge, arena, mid, high, (Measure *) r);

    return p->predicate(p, n);          // Chain to the result.
}


static Predicate * search_high(
    Arena * a, Predicate * x, uint64_t mid, uint64_t high, Measure * q)
{
    DeferredSearch2 * y = arena_alloc(a, BOUND_SEARCH2_BYTES);
    y->b.predicate = search2worker;
    y->arena = a;
    y->x = x;
    y->mid = mid;
    y->high = high;
    y->q = q;
    return (Predicate *) y;
}

static bool search1measure(Measure * me, Arena * a, Predicate * x)
{
    ResolvedSearch1 * s = (ResolvedSearch1 *) me;
    Predicate * merged = merge(
        a, s->mid, x, search_high(a, x, s->mid, s->high, s->q));
    return s->q->measure(s->q, a, merged);
}

static bool search1worker(Predicate * p, uint64_t n)
{
    // We transmute in place from a lazily deferred value to the computed value.
    // We have space for both.  (The call to range() below produces either
    // a merge or a constant, both of which we have left space for.)
    DeferredSearch1 * s = (DeferredSearch1 *) p;
    Arena * arena = s->arena;
    uint64_t low = s->low;
    uint64_t mid = s->mid;
    uint64_t high = s->high;
    Measure * q = s->q;

    ResolvedSearch1 * r = (ResolvedSearch1 *) p;
    r->m.measure = search1measure;
    r->mid = mid;
    r->high = high;
    r->q = q;

    range(&r->m.merge, arena, low, mid, (Measure *) r);

    return p->predicate(p, n);
}

static void split(
    BoundMerge * target,
    Arena * a, uint64_t low, uint64_t mid, uint64_t high, Measure * q)
{
    DeferredSearch1 * x = arena_alloc(a, BOUND_SEARCH1_BYTES);
    x->b.predicate = search1worker;
    x->arena = a;
    x->low = low;
    x->mid = mid;
    x->high = high;
    x->q = q;
    Predicate * y = search_high(a, (Predicate *) x, mid, high, q);
    target->b.predicate = merge_worker;
    target->pivot = mid;
    target->x = (Predicate *) x;
    target->y = y;
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

// We overload `high==0` to mean unbounded, unlike the versions in other
// languages, which split out that as a separate function `after`.
static void range(
    BoundMerge * target,
    Arena * a, uint64_t low, uint64_t high, Measure * q)
{
    if (high == low + 1) {
        target->b.predicate = q->measure(q, a, &constTrue)
            ? &returnTrue : &returnFalse;
        return;
    }

    uint64_t mid;
    if (high == 0)
        mid = low * 2 + 1;
    else
        mid = low + (high - low) / 2;

    split(target, a, low, mid, high, q);
}


static uint64_t limit(Predicate * p)
{
    uint64_t m = 0;
    uint64_t n = 1;
    while (p->predicate(p, n)) {
        m = n;
        n = 2 * m + 1;
    }
    while (n - m > 1) {
        uint64_t pivot = m + (n - m) / 2;
        if (p->predicate(p, pivot))
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

static bool negMeasure(Measure * m, Arena * a, Predicate * p)
{
    NegMeasure * n = (NegMeasure *) m;
    return !n->q->measure(n->q, a, p);
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
    Arena a;
    arena_init(&a);
    Predicate * merged = merge(&a, n, &Arbitrary, p->different);
    bool q_merged = p->q->measure(p->q, &a, merged);
    arena_free(&a);
    return q_merged != p->p_arbitrary;
}

typedef struct Special {
    Predicate b;
    Predicate * inner;
    uint64_t n;
    bool v;
} Special;

static bool special_worker(Predicate * p, uint64_t n)
{
    Special * s = (Special *) p;
    if (n == s->n)
        return s->v;
    else
        return s->inner->predicate(s->inner, n);
}

static Predicate * special(Arena * a, Predicate * p, uint64_t n, bool v)
{
    Special * s = arena_alloc(a, sizeof(Special));
    s->b.predicate = special_worker;
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

static bool specialize_worker(Measure * m, Arena * a, Predicate * p)
{
    Specialize * s = (Specialize *) m;
    Predicate * sp = special(a, p, s->n, s->v);
    return s->inner->measure(s->inner, a, sp);
}

static Measure * specialize(Arena * a, Measure * m, uint64_t n, bool v)
{
    Specialize * s = arena_alloc(a, sizeof(Specialize));
    s->b.measure = specialize_worker;
    s->inner = m;
    s->n = n;
    s->v = v;
    return (Measure *) s;
}

static const Raw * raw(Arena * ar, Measure * q)
{
    Arena a;
    arena_init(&a);
    bool q_arbitrary = q->measure(q, &a, &Arbitrary);
    arena_free(&a);
    struct NegMeasure negarg;
    negarg.b.measure = negMeasure;
    negarg.q = q;

    BoundMerge different;
    range(&different, &a, 0, 0, q_arbitrary ? (Measure *) &negarg : q);

    if (q->measure(q, &a, (Predicate *) &different) == q_arbitrary) {
        arena_free(&a);
        return q_arbitrary ? &RawT : &RawF;
    }

    Partial partial = {
        { partialF }, q, (Predicate *) &different, q_arbitrary };
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
static const Raw * dup(Arena * a, const Raw * r)
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
static const Raw * slice(Arena * a, const Raw * r, uint64_t pivot, bool v)
{
    if (r->tt == NULL)
        return r;
    if (r->pivot == pivot)
        return dup(a, v ? r->tt : r->ff);
    const Raw * tt = slice(a, r->tt, pivot, v);
    if (raw_eq_slice(tt, r->ff, pivot, v))
        return tt;

    Raw * node = arena_alloc(a, sizeof(Raw));
    node->pivot = r->pivot;
    node->tt = tt;
    node->ff = slice(a, r->ff, pivot, v);
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

static void weights(Arena * a, HashTable * ht, double w, const Raw * r)
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
    Arena a;
    arena_init(&a);
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


static const Raw * optimize(Arena * a, const Raw * r)
{
    if (r->tt == NULL)
        return r;
    uint64_t pivot = max_weight(r);
    Raw * node = arena_alloc(a, sizeof(Raw));
    node->pivot = pivot;
    Arena temp;
    arena_init(&temp);
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

static bool slow;

static bool martin(Measure * m, Arena * a, Predicate * p)
{
    int n = 0;
    if (p->predicate(p, 111111111111111))
        n += 1;
    if (p->predicate(p, 222222222222222))
        n += 2;
    if (p->predicate(p, 333333333333333))
        n += 4;
    if (p->predicate(p, 444444444444444))
        n += 8;
    if (slow && p->predicate(p, 888888888888888))
        n += 8;
    return p->predicate(p, n) != p->predicate(p, n+1);
}

static Measure Martin = { {{NULL}, 0, NULL, NULL}, martin };

int main(int argc, char * const argv[])
{
    if (argc > 1 && strcmp(argv[1], "slow") == 0)
        slow = true;
    Arena a;
    arena_init(&a);

    const Raw * r = raw(&a, &Martin);
    Arena b;
    arena_init(&b);
    r = optimize(&b, r);
    arena_free(&a);
    r = optimize(&a, r);
    arena_free(&b);
    cook(r);
    printf("\n");
    arena_free(&a);

    // Clear out the arena LRU.
    while (free_arenas) {
        Arena * it = free_arenas;
        free_arenas = it->chain;
        free(it);
    }
    return 0;
}
