
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#define DIM 4
#define DIM2 8


void gauss_elim( double a[DIM][DIM2] )
{
  int s,i,j,r;
  double det=1;
  for(s=0; s<DIM-1;++s) {

    /* partial pivoting : find max abs */
    #if 0
    double pv = fabs(a[s][s]);
    r=s;
    for(i=s+1; i<DIM; ++i) {
      double x = fabs(a[i][s]);
      if(pv < x) pv=x, r=i; 
    }
    #endif
    
    /* minimal pivoting : find nonzero */
    #if 1
    for (i=s; i<DIM; ++i)
        if (fabs(a[i][s])>1e-14) { r=i; break; }
    #endif

    if(r != s)
      for(j=0; j<DIM2; ++j) {
        double t = a[s][j];
        a[s][j]  = a[r][j];
        a[r][j]  = t;
      }
    for(i=s+1; i<DIM; ++i) {
      double f = -a[i][s]/a[s][s];
      for(j=s+1; j<DIM2; ++j)
        a[i][j] += a[s][j]*f;
    }
  }
}



void invert( double a[DIM][DIM] )
{
  /* create augmented matrix */
  int i,j,k;
  double e[DIM][DIM2];
  for(i=0;i<DIM;++i)
  for(j=0;j<DIM;++j) {
    e[i][j]   = a[i][j];
    e[i][DIM+j] = i==j?1:0;
  }

  gauss_elim(e);

  /* back substitution */
  for(i=DIM-1; i>=0; --i)
    for(j=0; j<DIM; ++j) {
      for(k=i+1; k<DIM; ++k)
        e[i][DIM+j] -= e[i][k]*e[k][DIM+j];
      e[i][DIM+j] /= e[i][i];
      a[i][j] = e[i][j+DIM];
    }
}

inline
void copy( double a[DIM][DIM], double b[DIM][DIM] )
{
  int i,j;
  for ( i=0; i<DIM; ++i )
  for ( j=0; j<DIM; ++j ) 
    a[i][j] = b[i][j];
}

double m3[3][3] =
    {{ 1, 2, 3 },
     { 4, 5, 6 },
     { 7, 8,10 }};

double m4[4][4] =
    {{ 1, 2, 3, 4},
     { 5, 6, 7, 7},
     { 9,10,11,12},
     {13,13,15,16}};

int main (int argc, char ** argv)
{
  int n = atoi(argv[1]);
  double (*a)[DIM][DIM] = malloc( n * sizeof(double[DIM][DIM]) );
  double (*b)[DIM][DIM] = malloc( n * sizeof(double[DIM][DIM]) );
  int i,j;
  for( i=0; i<n; ++i) copy(a[i],m4);
  for( i=0; i<n; ++i) copy(b[i],a[i]), invert(b[i]);

  for ( i=0; i<DIM; ++i ) {
    for ( j=0; j<DIM; ++j ) {
        printf("%lf,",(*b)[i][j]);
    }
    printf("\n");
  }
}

