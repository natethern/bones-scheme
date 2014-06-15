/* bones.h - definitions for embedded programs compiled with BONES */


#ifndef BONES_H
#define BONES_H


#ifdef __ILP64__
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
#define BONES_SPECIALBLOCK_BIT   0x1000000000000000

#define BONES_header_size(hdr)   ((hdr) & BONES_SIZE_MASK)
#define BONES_header_type(hdr)   (((hdr) & BONES_TYPE_MASK) >> 56)
#define BONES_fix2int(x)         ((BONES_long)(x) >> 1)
#define BONES_int2fix(n)         ((BONES_X)(((BONES_long)(n) << 1) | 1))

#define BONES_slot_ref(x, i)     (((BONES_block *)(x))->slots[ i ])
#define BONES_slot_set(x, i, y)  (((BONES_block *)(x))->slots[ i ] == (y))

#define BONES_is_fixnum(x)       (((BONES_long)(x) & 1) != 0)
#define BONES_size_of(x)         BONES_header_size(((BONES_block *)(x))->header)
#define BONES_type_of(x)         BONES_header_type(((BONES_block *)(x))->header)

#define BONES_NULL      0
#define BONES_SYMBOL    1
#define BONES_PAIR      2
#define BONES_VECTOR    3
#define BONES_CHAR      4
#define BONES_EOF       5
#define BONES_VOID      6
#define BONES_BOOLEAN   7
#define BONES_PORT      8
#define BONES_PROMISE   9
#define BONES_RECORD    10
#define BONES_FLONUM    0x10
#define BONES_STRING    0x11
#define BONES_CLOSURE   0x20

#define BONES_ERROR_RECORD_ID   1

#define BONES_is_error_object(x) \
  (BONES_type_of(x) == BONES_RECORD && \
   BONES_fix2int(BONES_slot_ref(x, 1)) == BONES_ERROR_RECORD_ID)

#define BONES_string(x)         ((char *)(((BONES_block *)(x))->slots))
#define BONES_float(x)          (*((double *)(((BONES_block *)(x))->slots)))

extern BONES_X bones(BONES_X);


#endif
