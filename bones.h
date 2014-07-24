/* bones.h - definitions for embedded programs compiled with BONES */


#ifndef BONES_H
#define BONES_H


#ifdef __LLP64__
typedef long long BONES_long;
#else
typedef long BONES_long;
#endif

typedef void *BONES_X;

typedef struct BONES_block {
  BONES_long header;
  BONES_X slots[ 1 ];
} BONES_block;

#define BONES_SIZE_MASK          0x00ffffffffffffff
#define BONES_TYPE_MASK          0x7f00000000000000
#define BONES_BYTEBLOCK_BIT      0x1000000000000000
#define BONES_SPECIALBLOCK_BIT   0x2000000000000000

/* basic types */
#define BONES_NULL      0L
#define BONES_SYMBOL    1L
#define BONES_PAIR      2L
#define BONES_VECTOR    3L
#define BONES_CHAR      4L
#define BONES_EOF       5L
#define BONES_VOID      6L
#define BONES_BOOLEAN   7L
#define BONES_PORT      8L
#define BONES_PROMISE   9L
#define BONES_RECORD    10L
#define BONES_FLONUM    0x10L
#define BONES_STRING    0x11L
#define BONES_CLOSURE   0x20L

#define BONES_ERROR_RECORD_ID   1

/* access block header */
#define BONES_header(x)          (((BONES_block *)(x))->header)
#define BONES_header_set(x, h)   (((BONES_block *)(x))->header = h)

/* operations on block-headers */
#define BONES_header_size(hdr)   ((hdr) & BONES_SIZE_MASK)
#define BONES_header_type(hdr)   (((hdr) & BONES_TYPE_MASK) >> 56)

/* fixnum <-> integer conversion */
#define BONES_fix2int(x)         ((BONES_long)(x) >> 1)
#define BONES_int2fix(n)         ((BONES_X)(((BONES_long)(n) << 1) | 1))

/* value predicates */
#define BONES_is_fixnum(x)       (((BONES_long)(x) & 1) != 0)
#define BONES_is_block(x)        (!BONES_is_fixnum(x))

/* block data accessors */
#define BONES_slot_ref(x, i)     (((BONES_block *)(x))->slots[ i ])
#define BONES_slot_set(x, i, y)  (((BONES_block *)(x))->slots[ i ] = (y))

/* these macros should not be used on a fixnum */
#define BONES_size_of(x)         BONES_header_size(BONES_header(x))
#define BONES_type_of(x)         BONES_header_type(BONES_header(x))

#define BONES_is_error_object(x)					\
  ({BONES_X ___x = (x);							\
    !BONES_is_fixnum(___x) && BONES_type_of(___x) == BONES_RECORD &&	\
      BONES_fix2int(BONES_slot_ref(___x, 1)) == BONES_ERROR_RECORD_ID;})

/* extract string-pointer and float-value */
#define BONES_string(x)         ((char *)(((BONES_block *)(x))->slots))
#define BONES_float(x)          (*((double *)(((BONES_block *)(x))->slots)))
#define BONES_bytevector(x)     ((unsigned char *)((BONES_block *)(x))->slots)

/* extract error-message fields */
#define BONES_error_object_message(eo)    BONES_slot_ref(eo, 2)
#define BONES_error_object_irritants(eo)  BONES_slot_ref(eo, 3)
#define BONES_error_object_location(eo)   BONES_slot_ref(eo, 4)


/* default entry-point */
extern BONES_X bones(BONES_X);


#endif
