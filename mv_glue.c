/* Interface to PHC's C translation of the software MixVol by Tangan Gao,
 * Tien-Yien Li, Li Xing, and Mengnien Wu. */

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include "prepare_for_mv.h"
#include "mixed_volume.h"

#define max(x,y) (x) > (y) ? (x) : (y)
#define min(x,y) (x) > (y) ? (x) : (y)

int quick_return ( int nVar, int *SptIdx, int **Spt );
/*
 * DESCRITION :
 *   Returns 1 if the system is univariate, or linear, or if some supports
 *   have fewer than two terms in them, printing a message;
 *   otherwise 0 is returned. */

int compute_mixed_volume (int nVar, int nPts, int *SptIdx, int **Spt,
			  CellStack *MCells )
{
   int i,j,p,nS,nSpt,CellSize,MVol,nbCells;
   int *SptType,*VtxIdx,**Vtx,*NuIdx2OldIdx;
   double *lft;

   if(quick_return(nVar,SptIdx,Spt) == 1) return -1;
   
   /* preprocessing phase */    
   SptType = (int*)calloc(nVar,sizeof(int));
   VtxIdx = (int*)calloc(nVar+1,sizeof(int));
   Vtx = (int**)calloc(nPts,sizeof(int*));
   for(i=0; i<nPts; i++) Vtx[i] = (int*)calloc(nVar,sizeof(int));
   NuIdx2OldIdx = (int*)calloc(nPts,sizeof(int));
   nSpt = nVar;
   Pre4MV(nVar,nSpt,&nS,SptType,Spt,SptIdx,Vtx,VtxIdx,NuIdx2OldIdx);
   nSpt = nS;
   /* end of preprocessing */

  /* srand((unsigned)(time(0))); */                /* apply a random lifting */
   srand(123323);
   lft = (double*)calloc(VtxIdx[nSpt],sizeof(double));
   for(j=0; j<VtxIdx[nSpt]; j++)
      lft[j] = 1.0+(3*(double)(rand())-(double)(rand()))/RAND_MAX;
#ifdef DEBUG
   printf("The lifting values :\n");
   for(i=0; i<VtxIdx[nSpt]; i++) printf("%2.15lf\n",lft[i]);
#endif       
   CellSize = cell_size(nSpt,SptType);
   Cs_Init(MCells,CellSize);
        
   MixedVol(nVar,nSpt,CellSize,SptType,VtxIdx,Vtx,lft,&nbCells,MCells,&MVol);

   /*
   write_cells(10,"cells.out",
                  nVar,nSpt,SptType,Vtx,lft,CellSize,nbCells,MCells);
   */
#ifdef DEBUG
   printf("The mixed volume of this support is %d.\n",MVol);
   printf("See the file %s",output_file);
   printf(" for a regular mixed-cell configuration.\n");
#endif

	
   free(VtxIdx);
   for(i=0; i<nPts; i++)
   {
     free(Vtx[i]);
   }
   free(Vtx);
   free(SptType);
   free(NuIdx2OldIdx);
   free(lft);
       
   return MVol;
}

void free_cells(CellStack *MCells)
{
   while(! Cs_IsEmpty(MCells)) Cs_Pop(MCells);
}

int quick_return ( int nVar, int *SptIdx, int **Spt )
{
   int i,j,k,kmin,kmax;

   k = -1;
   for(i=0; i<nVar; i++)
      if(SptIdx[i+1]-SptIdx[i] < 2)
      {
         k = i;
         break;
      }
   if(k >= 0)
   {
#ifdef DEBUG
      printf("The %d-th support has less than 2 points\n",k+1);
#endif
      return 1;   /* end of the case: too few terms */
   }
   if(nVar == 1)
   {
      kmin = INT_MAX/2;
      kmax = -INT_MAX/2;
      for(i=0; i<SptIdx[1]; i++) 
      {
         kmax = max(kmax,Spt[i][0]);
         kmin = min(kmin,Spt[i][0]);
      }
#ifdef DEBUG
      printf("Support is 1-dimensional with mixed volume %d\n",kmax-kmin);
#endif
      return 1;   /* end of the case: 1-variable */
   }
   for(i=0; i<SptIdx[nVar]; i++)
   {
      kmin = INT_MAX/2;
      kmax = -INT_MAX/2;
      k=0;
      for(j=0; j<nVar; j++)
      {
         kmax = max(kmax,Spt[i][j]);
         kmin = min(kmin,Spt[i][j]);
         k += abs(Spt[i][j]);
      }
      if(kmin != 0 || kmax > 1 || k > 1)
      {
         j = -1;
         break;
      }
   }
   if(j != -1)
   {
#ifdef DEBUG
      printf("A linear support, its mixed volume <= 1\n");
#endif
      return 1;   /* end of the case: linear system */
   } 
   return 0;      /* no quick return */
}
