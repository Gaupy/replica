#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glpk.h>
#include <assert.h>
#include <math.h>

#define LENGTH 200000 /* number of non-zero values in the LP formulation */
#define MAX_TASK 300
#define MAX_SERVERS 300
#define MAX_SPEEDS 100
#define NAME_LENGTH 200 /* name each variable and constraint */

/* I know global variables are harmful, but let's just use them. */
float precMatrix[MAX_TASK][MAX_TASK];
float serverSpeeds[MAX_SPEEDS];
double energy_static;
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
			if (serverSpeeds[k] == 0) 
			{
				glp_set_obj_coef(lp, col, 0);
			}
			else
			{
				glp_set_obj_coef(lp, col, serverSpeeds[k]*serverSpeeds[k]*serverSpeeds[k] + energy_static);
			}
			snprintf(name,NAME_LENGTH, "x_%02d_%02d", j, k);
			//fprintf(stderr,"x_%02d_%02d", j, k);
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

			assert(nnz<LENGTH);

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
	 	printf("\t-1");
/*	 	printf("No feasible Solution: %d\n", glp_mip_status(lp));*/
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
	char name[NAME_LENGTH];
	int row, col; /* dummy variables for indexing row and column */
	char *pch;
	FILE *fp,*fp2;
	int k,i,j;
	int t,s,l, iter;
	int expe_number;
	double result_greedy, result_move1, result_move2;
	int x;
	int spe;
	double y;
	float verif;
	if(argc != 6) {
       printf("Usage: %s number-of-rings\n",argv[0]);
       return 1;}
	t = atoi(argv[1]);
	s = atoi(argv[2]);
	spe = atoi(argv[3]);
	l = atoi(argv[4]);
	iter = atoi(argv[5]);
        sprintf(name, "size=%d_idle=%d_speeds=%d_expe=%d_iter=%d.dat", t, s, spe, l, iter);
	if ((fp = fopen(name, "r")) == NULL ) {
		printf("No results for size=%d_idle=%d_speeds=%d_expe=%d_iter=%d.dat \n", t, s, spe, l, iter);
		exit(EXIT_FAILURE);
	}

	assert(fp);
	fscanf(fp,"%d",&expe_number);
	int number_of_heur;
	fscanf(fp,"%d",&number_of_heur);
	int serv_number_heur[number_of_heur];
	double result_heur[number_of_heur];
	float verif_heur[number_of_heur];
	for (k=1; k<= number_of_heur; k++)
	{
		fscanf(fp,"%d",&serv_number_heur[k]); /*This is the number of servers used by heur[k]*/
		fscanf(fp,"%lf",&result_heur[k]); /* This is the result of heur[k]*/
		fscanf(fp,"%f",&verif_heur[k]); /*We verify here the precision of the result obtained by heur[k]*/
	}

	fscanf(fp,"%d",&K);
	K = K+1;
	fscanf(fp,"%d",&T);
	S = T;	/*The number of servers is not important anymore. */
	fscanf(fp,"%lf",&energy_static);
	for(k = 1; k <= K; k++)
	{
		fscanf(fp,"%lf",&y); 
		serverSpeeds[k] = y;
	}   

	verif = 0; /* Note that verif at the end is the number of requests processed by the tree.*/
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

/* If you have access to cplex, use this to solve the problem*/
	sprintf(name, "pbm_size=%d_idle=%d_speeds=%d_expe=%d_iter=%d_general.lp", t, s, spe, l, iter);
	int ret = glp_write_lp(lp, NULL, name);

/*	if (expe_number = 1)*/ /* When the expe number is 1, we study trees that have between 1 and t noeuds.*/
/*	{*/
/*		for (i = 1; i <= t; i++)*/
/*		{*/
/*			sprintf(name, "pbm_size=%d_idle=%d_expe=%d_iter=%d_general.lp", i, s, l, iter);*/
/*			int ret = glp_write_lp(lp, NULL, name);*/
/*		}*/
/*	}*/

/* This was used to solve with glpk instead of cplex; if you do not have an access to cplex decomment the two following lines.*/
/*	solve();*/
/*	show();*/
	clean();


/*This file is where we write the results obtained with the caml procedure and the verifications.*/
        sprintf(name, "result_%d_%d_%d_%d_%d.temp", t, s, spe, l, iter);
	fp2 = fopen(name,"w");
	if (fp2 == NULL) {
	  fprintf(stderr, "Can't open output file %s!\n", name);
	  exit(1);
	}

	fprintf(fp2,"%d \t %d \t %d \t%d \t %d \t %d \t", t, s, spe, l, iter, number_of_heur);

	for (k=1; k<= number_of_heur; k++)
	{
		fprintf(fp2,"%d \t",serv_number_heur[k]); /*This is the number of servers used by heur[k]*/
		fprintf(fp2,"%lf \t",result_heur[k]); /* This is the result of heur[k]*/
		fprintf(fp2,"%f \t",verif_heur[k]); /*We verify here the precision of the result obtained by heur[k]*/
	}
	fprintf(fp2,"%lf\t", verif); /*This is the load of the tree under study.*/

	fclose(fp2);
	fclose(fp);
	return 0;
}
