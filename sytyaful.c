// Note that you will need to raise your stack ulimit to run this with the
// `slow` option.
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#undef _NDEBUG
#include <assert.h>

#define MAX_ALIGN (_Alignof(max_align_t))
#define ARENA_BYTES 3968

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

// The measures we construct internally are all associated with a merge.  So we
// combine the structures instead of having two separate ones.
typedef struct Measure Measure;
typedef bool MeasureF(Measure * binding, Arena * a, Predicate * p);
struct Measure {
    BoundMerge merge;
    MeasureF * measure;
};

typedef struct DeferredSearchLow {
    Predicate b;
    Arena * arena;
    uint64_t low;
    uint64_t mid;
    uint64_t high;
    Measure * q;
} DeferredSearchLow;

typedef struct DeferredSearchHigh {
    Predicate b;
    Arena * arena;
    Predicate * x;
    uint64_t mid;
    uint64_t high;
    Measure * q;
} DeferredSearchHigh;

typedef struct ResolvedSearchLow {
    Measure m;
    uint64_t mid;
    uint64_t high;
    Measure * q;
} ResolvedSearchLow;

typedef struct ResolvedSearchHigh {
    Measure m;
    uint64_t mid;
    Predicate * x;
    Measure * q;
} ResolvedSearchHigh;

typedef union SearchLow {
    DeferredSearchLow deferred;
    ResolvedSearchLow resolved;
} SearchLow;

typedef union SearchHigh {
    DeferredSearchHigh deferred;
    ResolvedSearchHigh resolved;
} SearchHigh;

static bool range(BoundMerge * target,
                  Arena * a, uint64_t low, uint64_t high, Measure * q, uint64_t n);

static bool merge_worker(Predicate * p, uint64_t n)
{
    BoundMerge * m = (BoundMerge *) p;
    if (n < m->pivot)
        return m->x->predicate(m->x, n);
    else
        return m->y->predicate(m->y, n);
}

static Predicate * merge(
    BoundMerge * m, uint64_t pivot, Predicate * x, Predicate * y)
{
    m->b.predicate = merge_worker;
    m->pivot = pivot;
    m->x = x;
    m->y = y;
    return (Predicate *) m;
}

static bool search_high_measure(Measure * me, Arena * a, Predicate * y)
{
    ResolvedSearchHigh * s = (ResolvedSearchHigh *) me;
    BoundMerge m;
    return s->q->measure(s->q, a, merge(&m, s->mid, s->x, y));
}

static bool search_high_worker(Predicate * p, uint64_t n)
{
    // We transmute in place from a lazily deferred value to the resolved value.
    // We have space for either.  (The call to range() below produces either a
    // merge or a constant, both of which we have left space for.)
    DeferredSearchHigh * s = (DeferredSearchHigh *) p;
    Arena * arena = s->arena;
    Predicate * x = s->x;
    uint64_t mid  = s->mid;
    uint64_t high = s->high;
    Measure * q   = s->q;

    ResolvedSearchHigh * r = (ResolvedSearchHigh *) s;
    r->mid = mid;
    r->x = x;
    r->q = q;
    r->m.measure = search_high_measure;

    return range(&r->m.merge, arena, mid, high, (Measure *) r, n);

    //return p->predicate(p, n);          // Chain to the result.
}


static Predicate * search_high(
    SearchHigh * target, Arena * a,
    Predicate * x, uint64_t mid, uint64_t high, Measure * q)
{
    DeferredSearchHigh * y = &target->deferred;
    y->b.predicate = search_high_worker;
    y->arena = a;
    y->x = x;
    y->mid = mid;
    y->high = high;
    y->q = q;
    return (Predicate *) y;
}

static bool search_low_measure(Measure * me, Arena * a, Predicate * x)
{
    ResolvedSearchLow * s = (ResolvedSearchLow *) me;
    BoundMerge m;
    SearchHigh h;
    Predicate * merged = merge(
        &m, s->mid, x, search_high(&h, a, x, s->mid, s->high, s->q));
    return s->q->measure(s->q, a, merged);
}

static bool search_low_worker(Predicate * p, uint64_t n)
{
    // We transmute in place from a lazily deferred value to the resolved value.
    // We have space for either.  (The call to range() below produces either a
    // merge or a constant, both of which we have left space for.)
    DeferredSearchLow * s = (DeferredSearchLow *) p;
    Arena * arena = s->arena;
    uint64_t low = s->low;
    uint64_t mid = s->mid;
    uint64_t high = s->high;
    Measure * q = s->q;

    ResolvedSearchLow * r = (ResolvedSearchLow *) p;
    r->m.measure = search_low_measure;
    r->mid = mid;
    r->high = high;
    r->q = q;

    return range(&r->m.merge, arena, low, mid, (Measure *) r, n);
}

static bool split(BoundMerge * target, Arena * a,
                  uint64_t low, uint64_t mid, uint64_t high, Measure * q,
                  uint64_t n)
{
    // We need two records for the sub-searches, but allocate the space for both
    // in one go.
    struct Both { SearchLow low; SearchHigh high; };
    struct Both * both = arena_alloc(a, sizeof(struct Both));
    DeferredSearchLow * x = &both->low.deferred;
    x->b.predicate = search_low_worker;
    x->arena = a;
    x->low = low;
    x->mid = mid;
    x->high = high;
    x->q = q;
    Predicate * y = search_high(&both->high, a, (Predicate *) x, mid, high, q);
    merge(target, mid, (Predicate *) x, y);
    if (n == (uint64_t) -1)
        return false;
    if (n < mid)
        return search_low_worker((Predicate *) x, n);
    else
        return search_high_worker((Predicate *) y, n);
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
//
// The predicate is immediately run on n, if n != (uint64_t) -1.
static bool range(BoundMerge * target, Arena * a,
                  uint64_t low, uint64_t high, Measure * q, uint64_t n)
{
    if (high == low + 1) {
        bool b = q->measure(q, a, &constTrue);
        target->b.predicate = b ? &returnTrue : &returnFalse;
        return b;
    }

    uint64_t mid;
    if (high == 0)
        mid = low * 2 + 1;
    else
        mid = low + (high - low) / 2;

    return split(target, a, low, mid, high, q, n);
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
    BoundMerge m;
    Predicate * merged = merge(&m, n, &Arbitrary, p->different);
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

static Predicate * special(Special * target, Predicate * p, uint64_t n, bool v)
{
    target->b.predicate = special_worker;
    target->inner = p;
    target->n = n;
    target->v = v;
    return (Predicate *) target;
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
    Special sp;
    return s->inner->measure(s->inner, a, special(&sp, p, s->n, s->v));
}

static Measure * specialize(
    Specialize * target, Measure * m, uint64_t n, bool v)
{
    target->b.measure = specialize_worker;
    target->inner = m;
    target->n = n;
    target->v = v;
    return (Measure *) target;
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
    range(&different, &a, 0, 0, q_arbitrary ? (Measure *) &negarg : q, -1);

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

    Specialize sp;
    node->tt = raw(ar, specialize(&sp, q, pivot, true));
    node->ff = raw(ar, specialize(&sp, q, pivot, false));

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
    uint64_t k = 111111111111111;       // 15 1's.
    uint64_t n = 0;
    if (p->predicate(p, k))
        n += 1;
    if (p->predicate(p, 2*k))
        n += 2;
    if (p->predicate(p, 3*k))
        n += 4;
    if (p->predicate(p, 4*k))
        n += 8;
    if (slow && p->predicate(p, 5*k))
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
