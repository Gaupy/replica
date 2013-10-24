#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glpk.h>

#define LENGTH 100000 /* number of non-zero values in the LP formulation */
#define MAX_TASK 100
#define MAX_SERVERS 100
#define MAX_SPEEDS 100
#define NAME_LENGTH 20 /* name each variable and constraint */

/* I know global variables are harmful, but let's just use them. */
float precMatrix[MAX_TASK][MAX_TASK];
float serverSpeeds[MAX_SPEEDS];
int T; /* number of tree nodes */
int S; /* number of servers */
int K; /* number of speeds */

glp_prob *lp; /* linear programming object */

/* show the contents of precedence matrix. used only for debugging */
void show()
{
	int row, col;
	for (row = 1; row <= T; row++)
	{
		for (col = 1; col <= T; col++)
		{
			printf("%f\t", precMatrix[row][col]);
		}
		putchar('\n');
	}
}

void init()
{
	int ia[LENGTH], ja[LENGTH];
	double ar[LENGTH];
	char name[NAME_LENGTH];
	int i, j, k; /* dummy variables for indexing job1, job2, and chosen speed */
	int row, col; /* dummy variable for indexing row and column */
	int nnz = 0;  /* number of non-zero elements */ 
	/* create problem */
	lp = glp_create_prob();
	glp_set_prob_name(lp, "serverTrees"); /* name the LP */
	glp_set_obj_dir(lp, GLP_MIN);   /* minimize */

	/* add variables */
	glp_add_cols(lp, T*K + T*T);
	for (j = 1; j <= T; j++)
	{
		for (k = 1; k <= K; k++)
		{
			col = (j-1)*K + k;
			glp_set_col_kind(lp, col, GLP_BV); /* x_{j,k} \in \{0,1\} */
			glp_set_obj_coef(lp, col, serverSpeeds[k]*serverSpeeds[k]*serverSpeeds[k]);
			sprintf(name, "x_%02d_%02d", j, k);
			glp_set_col_name(lp, col, name);
		}
	}

	for (i = 1; i <= T; i++)
	{
		for (j=1; j <= T; j++)
		{
			col = T*K + (i-1)*T + j;
			glp_set_col_bnds(lp, col, GLP_LO, 0, 0); /* 0 <= y_{i,j} */
			glp_set_obj_coef(lp, col, 0);
			sprintf(name, "y_%02d_%02d", i,j);
			glp_set_col_name(lp, col, name);
		}
	}

	/* add constraints */
	glp_add_rows(lp, 4*T + 1); 
	for (row = 1; row <= T; row++)
	{
		/* maximum one server on each node <= 1 */
		glp_set_row_bnds(lp, row, GLP_UP, 0, 1);
		sprintf(name, "alpha_%02d", row);
		glp_set_row_name(lp, row, name);
	}
	for (row = T+1; row <= 2*T; row++)
	{
		/* the sum of that is executed of r(i) on the different nodes is equal to r(i) */
		glp_set_row_bnds(lp, row, GLP_FX, precMatrix[row-T][row -T], precMatrix[row-T][row -T]);
		sprintf(name, " beta_%02d", row - T);
		glp_set_row_name(lp, row, name);
	}
	for (row = 2*T+1; row <= 3*T; row++)
	{
		/* server does not propage too much 0 <= sum_k x_jk s(k) -sum_i y_ij */
		glp_set_row_bnds(lp, row, GLP_LO, 0, 0);
		sprintf(name, "gamma_%02d", row-2*T);
		glp_set_row_name(lp, row, name);
	}
	for (row = 3*T+1; row <= 4*T; row++)
	{
		/* sum y_ij when j \notin ancestors(i) = 0 */
		glp_set_row_bnds(lp, row, GLP_FX, 0, 0);
		sprintf(name, "delta_%02d", row-3*T);
		glp_set_row_name(lp, row, name);
	}
	for (row = 4*T+1; row <= 4*T+1; row++)
	{
		/* Maximum S servers */
		glp_set_row_bnds(lp, row, GLP_UP, 0, S);
		sprintf(name, "zeta");
		glp_set_row_name(lp, row, name);
	}



	/* fill up the region 1 */
	for (j = 1; j <= T; j++)
	{
		for (k = 1; k <= K; k++)
		{
			nnz++;
			ia[nnz] = j;
			ja[nnz] = (j-1)*K + k;
			ar[nnz] = 1;
		}
	}

	/* fill up the region 2 */
	for (i = 1; i <= T; i++)
	{
		for (j = 1; j <= T; j++)
		{
			if (precMatrix[j][i] != 0)
			{
				nnz++;
				row = T + i;
				col = T*K + (i-1)*T + j;
				ia[nnz] = row;
				ja[nnz] = col;
				ar[nnz] = 1;
			}
		}
	}

	/* fill up the region 3 */
	for (j = 1; j <= T; j++)
	{
		for (k = 1; k <= K; k++)
		{
			nnz++;
			row = 2*T + j;
			col = (j-1)*K + k;
			ia[nnz] = row;
			ja[nnz] = col;
			ar[nnz] = serverSpeeds[k];
		}
		for (i = 1; i <= T; i++)
		{
			nnz++;
			ia[nnz] = 2*T + j;
			ja[nnz] = T*K + (i-1)*T + j;
			ar[nnz] = -1;
		}
	}

	/* fill up the region 4 */
	for (i = 1; i <= T; i++)
	{
		for (j = 1; j <= T; j++)
		{
			if (precMatrix[j][i] == 0)
			{
				nnz++;
				ia[nnz] = 3*T + i;
				ja[nnz] = T*K + (i-1)*T + j;
				ar[nnz] = 1;
			}
		}
	}

	/* fill up the region 5 */
	for (j = 1; j <= T; j++)
	{
		for (k = 1; k <= K; k++)
		{
			nnz++;
			ia[nnz] = 4*T+1;
			ja[nnz] = (j-1)*K + k;
			ar[nnz] = 1;
		}
	}
	/* load the matrix */
	glp_load_matrix(lp, nnz, ia, ja, ar);
}

void solve()
{
	int j, k, i;
	int col;
	int jt, mt, bt, colt; /* temporary variables for indexing */
	double temp;
	int col_ind;

	glp_iocp param;
	glp_init_iocp(&param);
	
/*	param.presolve = GLP_ON;*/
/*	param.msg_lev = GLP_MSG_ERR;*/
	param.msg_lev = GLP_MSG_OFF;

	glp_smcp paramSimplex;
	glp_init_smcp(&paramSimplex);
	paramSimplex.msg_lev = GLP_MSG_OFF;
	


	glp_simplex(lp,&paramSimplex);
	glp_intopt(lp,&param);
	temp = glp_mip_obj_val(lp);
	if (glp_mip_status(lp) == GLP_OPT)
	{
	 	printf("\t %lf", temp);
	}
	else
	{
	 	printf("No feasible Solution: %d\n", glp_mip_status(lp));
	}
}

void clean()
{
	/* clean up */
	glp_delete_prob(lp);
	glp_free_env();
}

int main(int argc, char *argv[])
{
	int row, col; /* dummy variables for indexing row and column */
	char name[NAME_LENGTH];
	char *pch;
	FILE *fp;
	int k,i,j,t,s;
	double result_caml;
	int x;
	double y;
	float verif;
	if(argc != 4) {
       printf("Usage: %s number-of-rings\n",argv[0]);
       return 1;}
	t = atoi(argv[1]);
	s = atoi(argv[2]);
	i = atoi(argv[3]);
        sprintf(name, "size=%d_serv=%d_iter=%d.dat", t, s, i);
	if ((fp = fopen(name, "r")) == NULL ) {
/*		printf("No results for size=%d_serv=%d_iter=%d \n", t, s, i);*/
		exit(EXIT_FAILURE);
	}

	fp = fopen(name,"r");
	printf("%d \t %d \t %d \t", t, s,i);
	fscanf(fp,"%lf",&result_caml);
	printf("%lf", result_caml);
	fscanf(fp,"%d",&K);
	fscanf(fp,"%d",&T);
	fscanf(fp,"%d",&S);
	for(k = 1; k <= K; k++)
	{
		fscanf(fp,"%lf",&y); 
		serverSpeeds[k] = y;
	}   
	verif = 0;
	for (i = 1; i <= T;i++)
	{
		for(j = 1; j <= T;j++)
		{
			fscanf(fp,"%lf",&y); 
			precMatrix[i][j] = y;
		}   
	}
	for (i = 1; i <= T;i++)
	{
	verif += precMatrix[i][i];
	}

	init();
	solve();
/*	show();*/
	clean();

	for (i = 1; i <= T;i++)
	{
		for(j = 1; j <= T;j++)
		{
			fscanf(fp,"%lf",&y); 
			precMatrix[i][j] = y;
		}   
	}
	for (i = 1; i <= T;i++)
	{
	verif -= precMatrix[i][i];
	}

	if (-0.0001 <= verif && verif <= 0.0001)
	{
		init();
		solve();
/*		show();*/
		clean();
	}
	else
	{
		printf("\t-1");
	}
	printf("\t%f", verif);
	printf("\n");
	fclose(fp);
	return 0;
}
