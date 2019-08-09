#include <stdio.h>

/* Header files with a description of contents used */

#include <math.h>
#include <cvode/cvode.h>             /* prototypes for CVODE fcts., consts. */
#include <nvector/nvector_serial.h>  /* serial N_Vector types, fcts., macros */
#include <cvode/cvode_dense.h>       /* prototype for CVDense */
#include <sundials/sundials_dense.h> /* definitions DlsMat DENSE_ELEM */
#include <sundials/sundials_types.h> /* definition of type realtype */

/* Functions Called by the Solver */

static int ___f(realtype ___t, N_Vector ___y, N_Vector ___ydot, void *___user_data);

static int ___Jac(long int ___N, realtype ___t,
               N_Vector ___y, N_Vector ___fy, DlsMat ___J, void *___user_data,
               N_Vector ___tmp1, N_Vector ___tmp2, N_Vector ___tmp3);

/* Private functions to output results */

static void PrintOutput(realtype t);

/* Private function to print final statistics */

static void PrintFinalStats(void *cvode_mem);

/* Private function to check function return values */

static int check_flag(void *flagvalue, char *funcname, int opt);


/*
 *-------------------------------
 * Main Program
 *-------------------------------
 */

#define num_nodes _TEMPLATE_num_nodes
N_Vector ___ys[num_nodes];
double ___ts[num_nodes];
void * ___cvode_mems[num_nodes];

_TEMPLATE_parameters;
_TEMPLATE_debug_switch;

int main()
{
    //realtype ___tout;
  N_Vector ___abstol;
  int ___flag, ___flagr; //, ___iout;
  int nids[num_nodes];
  // TODO:1 :validate {:tags [:programming]} remove ___iout and replace with tdone condition
  const double ___t0 = 0.0;
  const double ___dt = _TEMPLATE_dt;
  const double ___num_time_steps = _TEMPLATE_num_time_steps;
  const long ___iterations_per_record = _TEMPLATE_iterations_per_record;
  const long ___NEQ = _TEMPLATE_num_vars; // Number of equations
  const double ___reltol = _TEMPLATE_relative_tolerance; /* Set the scalar relative tolerance */
  const char ___header[] = _TEMPLATE_igb_header_string;

  double ___tstep_end = ___dt;

  ___abstol = NULL;

  /* Create serial vector of length NEQ for I.C. and abstol */
  for (int n_idx = 0; n_idx<num_nodes; ++n_idx){
      nids[n_idx] = n_idx;
      ___ys[n_idx] = N_VNew_Serial(___NEQ);
      if (check_flag((void *)___ys[n_idx], "N_VNew_Serial", 0)) return(1);
      ___abstol = N_VNew_Serial(___NEQ); 
      if (check_flag((void *)___abstol, "N_VNew_Serial", 0)) return(1);
  }

  /*
  const int num_parameters
   */

  /* Initialize y */
  _TEMPLATE_initial_conditions;
  
  /* Set the vector absolute tolerance */
  _TEMPLATE_absolute_tolerances;

  for (int n_idx = 0; n_idx<num_nodes; ++n_idx){  
  /* Call CVodeCreate to create the solver memory and specify the 
   * Backward Differentiation Formula and the use of a Newton iteration */
  ___cvode_mems[n_idx] = CVodeCreate(CV_BDF, CV_NEWTON);
  if (check_flag((void *)___cvode_mems[n_idx], "CVodeCreate", 0)) return(1);
  
  /* Call CVodeInit to initialize the integrator memory and specify the
   * user's right hand side function in y'=f(t,y), the inital time T0, and
   * the initial dependent variable vector y. */
  ___flag = CVodeInit(___cvode_mems[n_idx], ___f, ___t0, ___ys[n_idx]);
  if (check_flag(&___flag, "CVodeInit", 1)) return(1);

  ___flag = CVodeSetUserData(___cvode_mems[n_idx], &(nids[n_idx]));
  if (check_flag(&___flag, "CVodeSetUserData", 1)) return(1);

  /* Call CVodeSVtolerances to specify the scalar relative tolerance
   * and vector absolute tolerances */
  ___flag = CVodeSVtolerances(___cvode_mems[n_idx], ___reltol, ___abstol);
  if (check_flag(&___flag, "CVodeSVtolerances", 1)) return(1);

  /* Call CVDense to specify the CVDENSE dense linear solver */
  ___flag = CVDense(___cvode_mems[n_idx], ___NEQ);
  if (check_flag(&___flag, "CVDense", 1)) return(1);

  /* Set the Jacobian routine to Jac (user-supplied) */
  ___flag = CVDlsSetDenseJacFn(___cvode_mems[n_idx], ___Jac);
  if (check_flag(&___flag, "CVDlsSetDenseJacFn", 1)) return(1);
  }

  /* In loop, call CVode, print results, and test for error.
     +MODIFIED+
     Break out of loop when NOUT preset output times have been reached.  */
  printf(" \n3-species kinetics problem\n\n");

  _TEMPLATE_igb_init;

  unsigned int ___failed_run = 0;
  unsigned int ___record_cycler[num_nodes];
  for (int n_idx = 0; n_idx<num_nodes;++n_idx){
      ___record_cycler[n_idx] = 1;
      _TEMPLATE_write_data;}
  for (long ts_idx = 1; ts_idx < ___num_time_steps; ++ts_idx){
      for (int n_idx = 0; n_idx<num_nodes; ++n_idx){
	      ___flag = CVode(___cvode_mems[n_idx], ___tstep_end, ___ys[n_idx], &(___ts[n_idx]), CV_NORMAL);

	      if (check_flag(&___flag, "CVode", 1) || ___flag != CV_SUCCESS){
		  ___failed_run = 1;
		  PrintOutput(___ts[n_idx]);
		  fprintf(stderr,"Node %d\n",n_idx);
		  _TEMPLATE_on_error;
		  break;}
	      else {
		  if (___record_cycler[n_idx] == ___iterations_per_record){
		      //printf("Write Data\n");
		      _TEMPLATE_write_data;
		      ___record_cycler[n_idx] = 1;}
		  else {
		      ++___record_cycler[n_idx];}}}
      if (___failed_run) break;
      else
	  ___tstep_end += ___dt;}

  printf("CVodeGetNumSteps:nst\n");
  printf("CVodeGetNumRhsEvals:nfe\n", 1);
  printf("CVodeGetNumLinSolvSetups:nsetups\n", 1);
  printf("CVodeGetNumErrTestFails:netf\n", 1);
  printf("CVodeGetNumNonlinSolvIters:nni\n", 1);
  printf("CVodeGetNumNonlinSolvConvFails:ncfn\n", 1);
  printf("CVDlsGetNumJacEvals:nje\n", 1);
  printf("CVDlsGetNumRhsEvals:nfeLS\n", 1);
  printf("CVodeGetNumGEvals:nge\n", 1);

  for (int n_idx = 0; n_idx<num_nodes; ++n_idx){
      /* Print some final statistics */
      printf("Node:%d\n",n_idx);
      PrintFinalStats(___cvode_mems[n_idx]);

      /* Free y and abstol vectors */
      N_VDestroy_Serial(___ys[n_idx]);
      
      /* Free integrator memory */
      CVodeFree(&(___cvode_mems[n_idx]));}

  N_VDestroy_Serial(___abstol);
  _TEMPLATE_close_igb;

  return(0);
}


/*
 *-------------------------------
 * Functions called by the solver
 *-------------------------------
 */

/*
 * f routine. Compute function f(t,y). 
 */

static int ___f(realtype ___t, N_Vector ___y, N_Vector ___ydot, void *___user_data)
{
    const int n_idx = *(int*)___user_data;
    //load sv
_TEMPLATE_load_sv;
_TEMPLATE_load_shared_sv;
#ifdef DEBUG_SV
 if (___t >= _TEMPLATE_debug_start && n_idx == _TEMPLATE_debug_node){
_TEMPLATE_debug_sv;
 }
#endif 
    //algebra bindings
_TEMPLATE_algebra_bindings;
    //differential bindings
_TEMPLATE_differential_bindings;
    return(0);
}

/*
 * Jacobian routine. Compute J(t,y) = df/dy. *
 */

static int ___Jac(long int ___N, realtype ___t,
               N_Vector ___y, N_Vector ___fy, DlsMat ___J, void *___user_data,
               N_Vector ___tmp1, N_Vector ___tmp2, N_Vector ___tmp3)
{
    const int n_idx = *(int*)___user_data;
_TEMPLATE_load_sv;
_TEMPLATE_load_shared_sv;
    //algebra bindings
_TEMPLATE_algebra_bindings;
    // Jacobian bindings
_TEMPLATE_jacobian_bindings;    
  return(0);
}

/*
 *-------------------------------
 * Private helper functions
 *-------------------------------
 */

static void PrintOutput(realtype t)
{
  printf("t:%0.4e\n", t);
  return;
}

/* 
 * Get and print some final statistics
 */

static void PrintFinalStats(void *cvode_mem)
{
  long int nst, nfe, nsetups, nje, nfeLS, nni, ncfn, netf, nge;
  int flag;

  flag = CVodeGetNumSteps(cvode_mem, &nst);
  check_flag(&flag, "CVodeGetNumSteps", 1);
  flag = CVodeGetNumRhsEvals(cvode_mem, &nfe);
  check_flag(&flag, "CVodeGetNumRhsEvals", 1);
  flag = CVodeGetNumLinSolvSetups(cvode_mem, &nsetups);
  check_flag(&flag, "CVodeGetNumLinSolvSetups", 1);
  flag = CVodeGetNumErrTestFails(cvode_mem, &netf);
  check_flag(&flag, "CVodeGetNumErrTestFails", 1);
  flag = CVodeGetNumNonlinSolvIters(cvode_mem, &nni);
  check_flag(&flag, "CVodeGetNumNonlinSolvIters", 1);
  flag = CVodeGetNumNonlinSolvConvFails(cvode_mem, &ncfn);
  check_flag(&flag, "CVodeGetNumNonlinSolvConvFails", 1);

  flag = CVDlsGetNumJacEvals(cvode_mem, &nje);
  check_flag(&flag, "CVDlsGetNumJacEvals", 1);
  flag = CVDlsGetNumRhsEvals(cvode_mem, &nfeLS);
  check_flag(&flag, "CVDlsGetNumRhsEvals", 1);

  flag = CVodeGetNumGEvals(cvode_mem, &nge);
  check_flag(&flag, "CVodeGetNumGEvals", 1);

  printf("\nFinal Statistics:\n");
  printf("nst = %-6ld nfe  = %-6ld nsetups = %-6ld nfeLS = %-6ld nje = %ld\n",
	 nst, nfe, nsetups, nfeLS, nje);
  printf("nni = %-6ld ncfn = %-6ld netf = %-6ld nge = %ld\n \n",
	 nni, ncfn, netf, nge);
}

/*
 * Check function return value...
 *   opt == 0 means SUNDIALS function allocates memory so check if
 *            returned NULL pointer
 *   opt == 1 means SUNDIALS function returns a flag so check if
 *            flag >= 0
 *   opt == 2 means function allocates memory so check if returned
 *            NULL pointer 
 */

static int check_flag(void *flagvalue, char *funcname, int opt)
{
  int *errflag;

  /* Check if SUNDIALS function returned NULL pointer - no memory allocated */
  if (opt == 0 && flagvalue == NULL) {
    fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed - returned NULL pointer\n\n",
	    funcname);
    return(1); }

  /* Check if flag < 0 */
  else if (opt == 1) {
    errflag = (int *) flagvalue;
    if (*errflag < 0) {
      fprintf(stderr, "\nSUNDIALS_ERROR: %s() failed with flag = %d\n\n",
	      funcname, *errflag);
      return(1); }}

  /* Check if function returned NULL pointer - no memory allocated */
  else if (opt == 2 && flagvalue == NULL) {
    fprintf(stderr, "\nMEMORY_ERROR: %s() failed - returned NULL pointer\n\n",
	    funcname);
    return(1); }

  return(0);
}
