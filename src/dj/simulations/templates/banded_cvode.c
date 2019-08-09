#include <stdio.h>
#include <stdlib.h>

/* Header files with a description of contents used */

#include <math.h>
#include <cvode/cvode.h>             /* prototypes for CVODE fcts., consts. */
#include <nvector/nvector_serial.h>  /* serial N_Vector types, fcts., macros */
#include <cvode/cvode_band.h>        /* prototype for CVBand */
#include <sundials/sundials_types.h> /* definition of type realtype */

/* Functions Called by the Solver */

static int ___f(realtype ___t, N_Vector ___y, N_Vector ___ydot, void *___user_data);

static int ___Jac(long int ___N, long int ___mu, long int ___ml,
               realtype ___t, N_Vector ___u, N_Vector ___fu, 
               DlsMat ___J, void *___user_data,
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
#define ___NEQ _TEMPLATE_num_vars // Number of equations

_TEMPLATE_parameters;
_TEMPLATE_debug_switch;

int main()
{
  int ___flag, ___flagr;
  N_Vector ___y, ___abstol;
  double ___t = 0.0;
  void * ___cvode_mem;

  const double ___t0 = 0.0;
  const double ___dt = _TEMPLATE_dt;
  const double ___num_time_steps = _TEMPLATE_num_time_steps;
  const long ___iterations_per_record = _TEMPLATE_iterations_per_record;
  const double ___reltol = _TEMPLATE_relative_tolerance; /* Set the scalar relative tolerance */
  const char ___header[] = _TEMPLATE_igb_header_string;

  double ___tstep_end = ___dt;

  /* Create serial vector of length NEQ for I.C. and abstol */
  ___y = N_VNew_Serial(___NEQ*num_nodes);
  if (check_flag((void *)___y, "N_VNew_Serial", 0)) return(1);
  ___abstol = N_VNew_Serial(___NEQ*num_nodes);
  if (check_flag((void *)___abstol, "N_VNew_Serial", 0)) return(1);

  /* Initialize y */
  _TEMPLATE_initial_conditions;
  for (int n_idx = 0; n_idx<num_nodes;++n_idx){
      _TEMPLATE_absolute_tolerance;}
  
  /* Call CVodeCreate to create the solver memory and specify the 
   * Backward Differentiation Formula and the use of a Newton iteration */
  ___cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
  if (check_flag((void *)___cvode_mem, "CVodeCreate", 0)) return(1);
  
  /* Call CVodeInit to initialize the integrator memory and specify the
   * user's right hand side function in y'=f(t,y), the inital time T0, and
   * the initial dependent variable vector y. */
  ___flag = CVodeInit(___cvode_mem, ___f, ___t0, ___y);
  if (check_flag(&___flag, "CVodeInit", 1)) return(1);

  ___flag = CVodeSVtolerances(___cvode_mem, ___reltol, ___abstol);
  if (check_flag(&___flag, "CVodeSVtolerances", 1)) return(1);

  ___flag = CVBand(___cvode_mem, ___NEQ*num_nodes, ___NEQ, ___NEQ);
  if (check_flag(&___flag, "CVBand", 1)) return(1);

  /* Set the Jacobian routine to Jac (user-supplied) */
  ___flag = CVDlsSetBandJacFn(___cvode_mem, ___Jac);
  if (check_flag(&___flag, "CVDlsSetBandJacFn", 1)) return(1);

  /* In loop, call CVode, print results, and test for error.
     +MODIFIED+
     Break out of loop when NOUT preset output times have been reached.  */

  _TEMPLATE_igb_init;

  unsigned int ___failed_run = 0;
  unsigned int ___record_cycler = 1;
  for (int n_idx = 0; n_idx<num_nodes;++n_idx){
      _TEMPLATE_write_data;}
  for (long ts_idx = 1; ts_idx < ___num_time_steps; ++ts_idx){
      ___flag = CVode(___cvode_mem, ___tstep_end, ___y, &___t, CV_NORMAL);

      if (check_flag(&___flag, "CVode", 1) || ___flag != CV_SUCCESS){
	  ___failed_run = 1;
	  PrintOutput(___t);
	  for (int n_idx = 0; n_idx<num_nodes;++n_idx){	 
	      _TEMPLATE_on_error;}
	  break;}
      else {
	  if (___record_cycler == ___iterations_per_record){
	  for (int n_idx = 0; n_idx<num_nodes;++n_idx){	 
	      _TEMPLATE_write_data;}
	  
	      ___record_cycler = 1;}
	  else {
	      ++___record_cycler;}}
      if (___failed_run) break;
      else
	  ___tstep_end += ___dt;}

  printf("CVodeGetNum...\n\n");
  printf("Steps:nst\n");
  printf("RhsEvals:nfe\n", 1);
  printf("LinSolvSetups:nsetups\n", 1);
  printf("ErrTestFails:netf\n", 1);
  printf("NonlinSolvIters:nni\n", 1);
  printf("NonlinSolvConvFails:ncfn\n", 1);
  printf("\nCVDlsGetNum...\n\n");
  printf("JacEvals:nje\n", 1);
  printf("RhsEvals:nfeLS\n", 1);

      PrintFinalStats(___cvode_mem);

      /* Free y and abstol vectors */
      N_VDestroy_Serial(___y);
      
      /* Free integrator memory */
      CVodeFree(&(___cvode_mem));

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
    for (int n_idx = 0; n_idx < num_nodes; ++n_idx){
    //load sv
_TEMPLATE_load_sv;
#ifdef DEBUG_SV
 if (___t >= _TEMPLATE_debug_start && n_idx == _TEMPLATE_debug_node){
_TEMPLATE_debug_sv;
 }
#endif 
    //algebra bindings
_TEMPLATE_algebra_bindings;
    //differential bindings
_TEMPLATE_differential_bindings;
    }
    return(0);
}

/*
 * Jacobian routine. Compute J(t,y) = df/dy. *
 */

static int ___Jac(long int ___N, long int ___mu, long int ___ml,
               realtype ___t, N_Vector ___y, N_Vector ___fu, 
               DlsMat ___J, void *___user_data,
               N_Vector ___tmp1, N_Vector ___tmp2, N_Vector ___tmp3)
{
    for (int n_idx = 0; n_idx < num_nodes; ++n_idx){
    // load algebra
_TEMPLATE_load_sv;
_TEMPLATE_algebra_bindings;
    // Jacobian bindings
_TEMPLATE_jacobian_bindings;
    }
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
    long int nst, nfe, nsetups, nje, nfeLS, nni, ncfn, netf;
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

  printf("\nFinal Statistics:\n");
  printf("nst = %-6ld nfe  = %-6ld nsetups = %-6ld nfeLS = %-6ld nje = %ld\n",
	 nst, nfe, nsetups, nfeLS, nje);
  printf("nni = %-6ld ncfn = %-6ld netf = %-6ld\n \n",
	 nni, ncfn, netf);
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
