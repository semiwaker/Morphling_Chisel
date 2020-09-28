#include "CGRA.h"

#define Pm 2
#define Pn 16
#define VEC_LENGTH 16
#define DATA_PACK 4
#define INDEX_PACK 4
#define DVU_LENGTH 64
#define SP_BlOCK_A_LENGTH 4
#define SP_BlOCK_B_LENGTH 4
#define MATRIX_ROW 256
#define MATRIX_COLUMN 256
#define DOU_LENGTH 256
#define FU_LENGTH 256
#define FU_MUL_NUM 16
#define DOU_INSTR_LENGTH 16
#define DAU_READ_STEP 4
#define OUT_TILE_SIZE 6
#define TENSOR_TILE_SIZE 65536 / 4 //256 x 256

template <typename T1>
void data_access_unit(quad_t router2PE *, quad_t switch2PE *,
					  T veg_reg[2][VEC_LENGTH],
					  uint col_idx_vec[2][VEC_LENGTH],
					  INSTR vec_instr *, short &instr_offset)
{
#pragma HLS inline off

#pragma HLS array_partition variable = veg_reg1 cyclic complete dim = 0
#pragma HLS array_partition variable = veg_reg2 cyclic complete dim = 0

	for (char i = 0; i < 2; ++i)
	{

		switch (vec_instr[i].dau_instr.vec_source)
		//vec read from router
		case 0:
		{
			for (char k = 0; k < VEC_LENGTH; k += DATA_PACK)
			{
#pragma HLS PIPELINE
				veg_reg[i][k + 0] = router2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in1;
				veg_reg[i][k + 1] = router2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in2;
				veg_reg[i][k + 2] = router2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in3;
				veg_reg[i][k + 3] = router2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in4;
			}
			instr_offset++;
			break;
		}

		//vec read from switch
		case 1:
		{
			for (char k = 0; k < VEC_LENGTH; k += DATA_PACK)
			{
#pragma HLS PIPELINE
				veg_reg[i][k + 0] = switch2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in1;
				veg_reg[i][k + 1] = switch2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in2;
				veg_reg[i][k + 2] = switch2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in3;
				veg_reg[i][k + 3] = switch2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in4;
			}
			instr_offset++;
			break;
		}

		//read for sparse
		case 2:
		{
			//sparse vec from matrix A
			//sparse matrix is stored in BCSR
			if (i == 0)
			{
				for (char k = 0; k < SP_BlOCK_A_LENGTH; k += DATA_PACK)
				{
#pragma HLS PIPELINE
					//offset is the row pointer of the matrix A
					veg_reg[i][k + 0] = router2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in1;
					veg_reg[i][k + 1] = router2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in2;
					veg_reg[i][k + 2] = router2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in3;
					veg_reg[i][k + 3] = router2PE[vec_instr[instr_offset].dau_instr.offset + k].data.in4;
				}
				instr_offset++;

				for (char k = 0; k < SP_BlOCK_A_LENGTH; k += INDEX_PACK)
				{
#pragma HLS PIPELINE
					col_idx_vec[i][k + 0] = router2PE[vec_instr[instr_offset].dau_instr.offset + k].index.idx1;
					col_idx_vec[i][k + 1] = router2PE[vec_instr[instr_offset].dau_instr.offset + k].index.idx2;
				}
				instr_offset++;
			}
			//sparse vec from matrix A
			else
			{ // i == 1
				for (char j = 0; j < SP_BlOCK_A_LENGTH; ++j)
				{
					for (char k = 0; k < SP_BlOCK_B_LENGTH; k += DATA_PACK)
					{
#pragma HLS PIPELINE
						//offset is the start element of matrix B
						veg_reg[i][k + 0] = router2PE[vec_instr[instr_offset].dau_instr.offset + col_idx_vec[0][j] * MATRIX_COLUMN + k].data.in1;
						veg_reg[i][k + 1] = router2PE[vec_instr[instr_offset].dau_instr.offset + col_idx_vec[0][j] * MATRIX_COLUMN + k].data.in2;
						veg_reg[i][k + 2] = router2PE[vec_instr[instr_offset].dau_instr.offset + col_idx_vec[0][j] * MATRIX_COLUMN + k].data.in3;
						veg_reg[i][k + 3] = router2PE[vec_instr[instr_offset].dau_instr.offset + col_idx_vec[0][j] * MATRIX_COLUMN + k].data.in4;
					}
				}

				instr_offset++;

				for (char j = 0; j < SP_BlOCK_A_LENGTH; ++j)
				{
					for (char k = 0; k < SP_BlOCK_B_LENGTH; k += INDEX_PACK)
					{
#pragma HLS PIPELINE
						col_idx_vec[i][k + j * SP_BlOCK_B_LENGTH + 0] = router2PE[vec_instr[instr_offset].dau_instr.offset + k + j * SP_BlOCK_B_LENGTH].index.idx1;
						col_idx_vec[i][k + j * SP_BLOCK_B_LENGTH + 1] = router2PE[vec_instr[instr_offset].dau_instr.offset + k + j * SP_BlOCK_B_LENGTH].index.idx2;
					}
				}

				instr_offset++;
			}
			break;
		}

		default:
			break;
	}
}

template <typename T>
void column_index_cmp(T dau_vec[2][VEC_LENGTH],
					  uint col_idx_vec[2][VEC_LENGTH],
					  T col_cmp_reg[2][VEC_LENGTH])
{
}

template <typename T>
void data_vectorization_unit(T dau_vec[2][VEC_LENGTH],
							 T dvu_vec[2][DAU_LENGTH],
							 uint col_idx_vec[2][VEC_LENGTH],
							 INSTR vec_instr *, short &instr_offset)
{

	T col_cmp_reg[VEC_LENGTH];

	for (char i = 0; i < 2; ++i)
	{

		short count = 0;
		//dense tensor operation
		if (vec_instr[instr_offset].dvu_instr.sparse)
		{
			// 	circular vectorization
			if (vec_instr[instr_offset].dvu_instr.type == 0)
			{
				for (short j = 0; j < vec_instr[instr_offset].times; ++j)
				{
#pragma HLS unroll
					for (short k = 0 < k < vec_instr[instr_offset].length; ++k)
					{
						dvu_vec[i][count++] = dau_vec[i][k];
					}
				}
				instr_offset++;
			}
			// sequential
			else
			{
				for (short k = 0 < k < vec_instr[instr_offset].length; ++k)
				{
#pragma HLS unroll
					for (short j = 0; j < vec_instr[instr_offset].times; ++j)
					{
						dvu_vec[i][count++] = dau_vec[i][k];
					}
				}
				instr_offset++;
			}
		}

		//sparse
		else
		{
			//vec A
			column_index_cmp(dau_vec, col_idx_vec, col_cmp_reg);
			if (vec_instr[instr_offset].dvu_instr.type == 0)
			{
			}
		}
	}
}
