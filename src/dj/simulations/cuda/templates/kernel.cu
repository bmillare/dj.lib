extern "C"
__global__ void __kernel_name(float *arg_dt, int *arg_rest, float *block_parameters, float *idata, float *sdata, float *odata)
{
    // The kernel needs to allocate all the shared memory of an SM
    // for each block so that only 1 block runs on an SM

    // Need to be able to determine what the shared memory size is.
    extern __shared__ float local_sdata[];

    // Array size is dependent on nodes * (sv + trace var) * record_time_steps
    unsigned int n_idx = threadIdx.x;
    unsigned int n_mem_idx = blockIdx.x*blockDim.x + threadIdx.x;
    unsigned int num_blocks = gridDim.x;
    const unsigned int num_vars = __num_vars;
    const unsigned int num_track_vars = __num_track_vars;
    const unsigned int num_shared_vars = __num_shared_vars;

    // runtime settable
    const int num_nodes = arg_rest[0];
    const int num_time_steps = arg_rest[1];
    const int iterations_per_record = arg_rest[2];
    const int num_parameters = arg_rest[3];
    const float dt = arg_dt[0];
    unsigned int parameter_offset = blockIdx.x*num_parameters;

    __set_block_parameters;

    // Shared State Variable Indexes
    __shared_sv_indexes;
    
    // State Variable Declarations
    __state_variable_declarations;

    // shared algebra declarations
    __shared_algebra_declarations;
    
    // Initial Conditions
    __initial_conditions;

    // Compute Loop
    unsigned int ts_idx;
    unsigned int record_cycler = 1;
    unsigned int record_ts_idx;
    if (n_idx < num_nodes)
	for (ts_idx = 1; ts_idx < num_time_steps; ++ts_idx)
	    {
		/// Read Shared Data
		// Note: We put only set from the values we care about
		//__threadfence_block();
		__shared_sv_read;

		// compute
		__compute;

		// Write Shared Data
		__shared_sv_write;
		    
		// Record Data
		if (record_cycler == iterations_per_record)
		    {
			record_ts_idx = ts_idx/iterations_per_record;
			__write_data;
			record_cycler = 1;
		    }
		else
		    {
			++record_cycler;
		    }
	    }
}
