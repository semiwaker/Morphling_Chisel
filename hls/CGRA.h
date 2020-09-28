#include "ap_fixed.h"
#include "math.h"
#include "ap_int.h"
//#include "hls_half.h"
//#include "hls_double.h"
#include "math.h"

extern "C" float sqrtf(float);

#define FIXED_NUMBER
#ifdef FIXED_NUMBER
typedef ap_fixed<16,6> data_t;
typedef ap_uint<16> data_instr;
#else
typedef float data_t;
typedef float data_instr;
#endif

//not use in[4] for HLS
typedef struct{
	data_t in1, in2, in3, in4;
} quad_data;

typedef struct{
	int idx1, idx2;
} sparse_idx;

typedef union{
  
  quad_data data;
  sparse_idx index;

} quad_t;

typedef struct{
	char vec_source;
	// 0 from router
	// 1 from switch
	// 2 from router for sparse
	ap_uint<12> offset; 
} DAU_INSTR_TYPE;

typedef struct{
	char sparse;
	char length;
	char type;
	char times;
} DVU_INSTR_TYPE;

typedef union{
  
  DAU_INSTR_TYPE dau_instr;
  DOU_INSTR_TYPE dvu_instr;

} INSTR;



