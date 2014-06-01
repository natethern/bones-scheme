/* bones.h - definitions for embedded programs compiled with BONES */


typedef void *BONES_X;

typedef struct BONES_block {
  long header;
  BONES_X slots[ 1 ];
} BONES_block;

#define BONES_SIZE_MASK          0x00ffffffffffffff
#define BONES_TYPE_MASK          0x7f00000000000000
#define BONES_BYTEBLOCK_BIT      0x1000000000000000
#define BONES_SPECIALBLOCK_BIT   0x1000000000000000

#define BONES_header_size(hdr)   ((hdr) & BONES_SIZE_MASK)
#define BONES_header_type(hdr)   (((hdr) & BONES_TYPE_MASK) >> 56)
#define BONES_fix2int(x)         ((long)(x) >> 1)
#define BONES_int2fix(n)         ((BONES_X)(((long)(n) << 1) | 1))

#define BONES_isfixnum(x)        (((long)(x) & 1) != 0)

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

extern BONES_X bones(BONES_X);
