#include <stdio.h>

/* Header files with a description of contents used */

#include <cvode/cvode.h>             /* prototypes for CVODE fcts., consts. */
#include <nvector/nvector_serial.h>  /* serial N_Vector types, fcts., macros */
#include <cvode/cvode_dense.h>       /* prototype for CVDense */
#include <sundials/sundials_dense.h> /* definitions DlsMat DENSE_ELEM */
#include <sundials/sundials_types.h> /* definition of type realtype */

/* User-defined vector and matrix accessor macros: Ith, IJth */

/* These macros are defined in order to write code which exactly matches
   the mathematical problem description given above.

   Ith(v,i) references the ith component of the vector v, where i is in
   the range [1..NEQ] and NEQ is defined below. The Ith macro is defined
   using the N_VIth macro in nvector.h. N_VIth numbers the components of
   a vector starting from 0.

*/

#define Ith(v,i)    NV_Ith_S(v,i-1)       /* Ith numbers components 1..NEQ */

/* Problem Constants */
#define _T0    RCONST(0.0)      /* initial time           */
#define _T1    RCONST(0.4)      /* first output time      */
#define _TMULT RCONST(10.0)     /* output time factor     */
#define _NOUT  12               /* number of output times */


/* Functions Called by the Solver */

static int _f(realtype _t, N_Vector _y, N_Vector _ydot, void *_user_data);

static int _Jac(long int _N, realtype _t,
               N_Vector _y, N_Vector _fy, DlsMat _J, void *_user_data,
               N_Vector _tmp1, N_Vector _tmp2, N_Vector _tmp3);

/* Private functions to output results */

static void PrintOutput(realtype t, realtype y1, realtype y2, realtype y3);

/* Private function to print final statistics */

static void PrintFinalStats(void *cvode_mem);

/* Private function to check function return values */

static int check_flag(void *flagvalue, char *funcname, int opt);


/*
 *-------------------------------
 * Main Program
 *-------------------------------
 */

int main()
{
  realtype _t, _tout;
  N_Vector _y, _abstol, _algebra_cache;
  void *_cvode_mem;
  int _flag, _flagr, _iout;
  const long _NEQ = __num_vars; // Number of equations
  const long _NALG = __num_algebra; // Number of algebra
  const double _reltol = __relative_tolerance; /* Set the scalar relative tolerance */

  _y = _abstol = _algebra_cache = NULL;
  _cvode_mem = NULL;

  /* Create serial vector of length NEQ for I.C. and abstol */
  _y = N_VNew_Serial(_NEQ);
  _algebra_cache = N_VNew_Serial(_NALG);
  if (check_flag((void *)_y, "N_VNew_Serial", 0)) return(1);
  _abstol = N_VNew_Serial(_NEQ); 
  if (check_flag((void *)_abstol, "N_VNew_Serial", 0)) return(1);

  /*
  unsigned int n_idx = threadIdx.x;
  unsigned int n_mem_idx = blockIdx.x*blockDim.x + threadIdx.x;
  unsigned int num_blocks = gridDim.x;
  const unsigned int num_vars = __num_vars;
  const unsigned int num_track_vars = __num_track_vars;
  const unsigned int num_shared_vars = __num_shared_vars;
  */

  /*
  const int num_nodes
  const int num_record_steps
  const int num_parameters
  const float dt
   */

  /* Initialize y */
  __initial_conditions;

  /* Set the vector absolute tolerance */
  __absolute_tolerances;

  /* Call CVodeCreate to create the solver memory and specify the 
   * Backward Differentiation Formula and the use of a Newton iteration */
  _cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
  if (check_flag((void *)_cvode_mem, "CVodeCreate", 0)) return(1);
  
  /* Call CVodeInit to initialize the integrator memory and specify the
   * user's right hand side function in y'=f(t,y), the inital time T0, and
   * the initial dependent variable vector y. */
  _flag = CVodeInit(_cvode_mem, _f, _T0, _y);
  if (check_flag(&_flag, "CVodeInit", 1)) return(1);

  _flag = CVodeSetUserData(_cvode_mem, _algebra_cache);
  if (check_flag(&_flag, "CVodeSetUserData", 1)) return(1);

  /* Call CVodeSVtolerances to specify the scalar relative tolerance
   * and vector absolute tolerances */
  _flag = CVodeSVtolerances(_cvode_mem, _reltol, _abstol);
  if (check_flag(&_flag, "CVodeSVtolerances", 1)) return(1);

  /* Call CVDense to specify the CVDENSE dense linear solver */
  _flag = CVDense(_cvode_mem, _NEQ);
  if (check_flag(&_flag, "CVDense", 1)) return(1);

  /* Set the Jacobian routine to Jac (user-supplied) */
  _flag = CVDlsSetDenseJacFn(_cvode_mem, _Jac);
  if (check_flag(&_flag, "CVDlsSetDenseJacFn", 1)) return(1);

  /* In loop, call CVode, print results, and test for error.
     Break out of loop when NOUT preset output times have been reached.  */
  printf(" \n3-species kinetics problem\n\n");

  _iout = 0;  _tout = _T1;
  while(1) {
    _flag = CVode(_cvode_mem, _tout, _y, &_t, CV_NORMAL);
    PrintOutput(_t, Ith(_y,1), Ith(_y,2), Ith(_y,3));

    if (check_flag(&_flag, "CVode", 1)) break;
    if (_flag == CV_SUCCESS) {
      _iout++;
      _tout *= _TMULT;
    }

    if (_iout == _NOUT) break;
  }

  /* Print some final statistics */
  PrintFinalStats(_cvode_mem);

  /* Free y and abstol vectors */
  N_VDestroy_Serial(_y);
  N_VDestroy_Serial(_abstol);

  /* Free integrator memory */
  CVodeFree(&_cvode_mem);

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

static int _f(realtype _t, N_Vector _y, N_Vector _ydot, void *_user_data)
{
    //load sv
__load_sv;   
    //algebra bindings
__algebra_bindings;
    //save algebra bindings
 N_Vector _algebra_cache = _user_data;
__save_algebra;
    //differential bindings
__differential_bindings;
    return(0);
}

/*
 * Jacobian routine. Compute J(t,y) = df/dy. *
 */

static int _Jac(long int _N, realtype _t,
               N_Vector _y, N_Vector _fy, DlsMat _J, void *_user_data,
               N_Vector _tmp1, N_Vector _tmp2, N_Vector _tmp3)
{
    // load algebra
 N_Vector _algebra_cache = _user_data;
__load_algebra;
    // Jacobian bindings
__jacobian_bindings;    
  return(0);
}

/*
 *-------------------------------
 * Private helper functions
 *-------------------------------
 */

static void PrintOutput(realtype t, realtype y1, realtype y2, realtype y3)
{
  printf("At t = %0.4e      y =%14.6e  %14.6e  %14.6e\n", t, y1, y2, y3);
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
