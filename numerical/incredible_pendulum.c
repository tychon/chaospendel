#define _GNU_SOURCE

#include <CL/opencl.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <sys/types.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>
#include <math.h>

#define SEPARATOR "----------------------------------------------------------------------\n"

#define use_gpu 1

// CL_SUCCESS is 0

// WARNING: The incredible pendulum can't handle double-precision
// floating-point values, it can only handle single precision!

// not usable in combination with signal handlers!
// (because we assume that reads don't fail in the
// middle)
char *slurp_file(char *path) {
  int fd = open(path, O_RDONLY);
  if (fd == -1) {
    fprintf(stderr, "file \"%s\" couldn't be opened: %s\n", path, strerror(errno));
    exit(1);
  }
  
  struct stat st;
  if (fstat(fd, &st)) {
    fprintf(stderr, "could not stat \"%s\": %s\n", path, strerror(errno));
    exit(1);
  }
  
  char *buf = malloc(st.st_size+1);
  buf[st.st_size] = '\0';
  if (buf == NULL) {
    perror("unable to allocate memory for file slurping");
    exit(1);
  }
  
  if (read(fd, buf, st.st_size) != st.st_size) {
    perror("unable to slurp some file at once");
    exit(1);
  }
  
  close(fd);
  return buf;
}

int main(int argc, char **argv) {
  int err;
  
  // ######  Parse args.  ######
  if (argc != 4) {
    fprintf(stderr, "wrong invocation: want width, height, outpath!\n");
    exit(1);
  }
  int width = atoi(argv[1]);
  int height = atoi(argv[2]);
  char *outpath = argv[3];
  
  
  // ######  Prepare the OpenCL environment and our kernel.  ######
  printf("preparing the OpenCL environment and the kernel...\n");
  cl_device_id device_id;
  err = clGetDeviceIDs(NULL, use_gpu?CL_DEVICE_TYPE_GPU:CL_DEVICE_TYPE_CPU, 1, &device_id, NULL);
  if (err) {
    printf("Error: Failed to locate a compute device!\n");
    return 1;
  }
  
  size_t max_workgroup_size;
  cl_char vendor_name[1024] = {0};
  cl_char device_name[1024] = {0};
  err = clGetDeviceInfo(device_id, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(size_t), &max_workgroup_size, NULL);
  err|= clGetDeviceInfo(device_id, CL_DEVICE_VENDOR, sizeof(vendor_name), vendor_name, NULL);
  err|= clGetDeviceInfo(device_id, CL_DEVICE_NAME, sizeof(device_name), device_name, NULL);
  if (err) {
    printf("Error: Failed to retrieve device info!\n");
    return 1;
  }
  
  printf("Connecting to %s %s...\n", vendor_name, device_name);
  
  cl_context context = clCreateContext(0, 1, &device_id, NULL, NULL, &err);
  if (context == NULL) {
    printf("Error: Failed to create a compute context!\n");
    return 1;
  }
  
  cl_command_queue commands = clCreateCommandQueue(context, device_id, 0, &err);
  if (commands == NULL) {
    printf("Error: Failed to create a command commands!\n");
    return 1;
  }
  
  const char *source = slurp_file("incredible_pendulum.cl");
  cl_program prog = clCreateProgramWithSource(context, 1, &source, NULL, &err);
  if (prog == NULL) {
    printf("Error: Failed to create compute program!\n");
    return 1;
  }
  
  err = clBuildProgram(prog, 0, NULL, NULL, NULL, NULL);
  if (err) {
    printf("Error: Failed to build program executable!\n");
    char build_log[2048];
    clGetProgramBuildInfo(prog, device_id, CL_PROGRAM_BUILD_LOG, sizeof(build_log), build_log, NULL);
    printf("%s\n", build_log);
    return 1;
  }
  
  cl_kernel kern = clCreateKernel(prog, "simulate_pendulum", &err);
  if (kern == NULL) {
    printf("Error: Failed to create compute kernel!\n");
    return 1;
  }
  
  
  // ######  Our kernel is ready, now prepare the input/output buffers and data.  ######
  printf("kernel is ready, preparing outfile, data and buffers...\n");
  size_t number_of_runs = width * height;
  cl_float *inputdata = malloc(2*sizeof(cl_float)*number_of_runs+4);
  inputdata = (cl_float*)(((long long)(inputdata+sizeof(cl_float)))&~0x3);
  cl_float *inputdata_ = inputdata;
  if (inputdata == NULL) {
    printf("Error: Failed to malloc input buffer!\n");
    return 1;
  }
  for (int y=0; y<height; y++) {
    for (int x=0; x<width; x++) {
      cl_float phi1 = (M_PI/2)*(1-y/(cl_float)height);
      *(inputdata_++) = phi1;
      cl_float phi2 = M_PI*(x/(cl_float)width);
      *(inputdata_++) = phi2;
    }
  }
  cl_mem cl_inbuf = clCreateBuffer(context, CL_MEM_READ_ONLY|CL_MEM_USE_HOST_PTR, 2*sizeof(cl_float)*number_of_runs, inputdata, NULL);
  if (cl_inbuf == NULL) {
    printf("Error: Failed to allocate OpenCL input buffer!\n");
    return 1;
  }
  err = clSetKernelArg(kern, 0, sizeof(cl_inbuf), &cl_inbuf);
  if (err) {
    printf("Error: Can't set kernel arg 0!\n");
    return 1;
  }
  
  int outfd = open(outpath, O_RDWR|O_CREAT|O_TRUNC, 0666);
  if (outfd == -1) {
    perror("can't open outfile");
    return 1;
  }
  char *header = NULL;
  asprintf(&header, "P6\n%d %d\n255\n", width, height);
  if (header == NULL) exit(1);
  if (write(outfd, header, strlen(header)) != strlen(header)) {
    printf("Error: Can't write outfile header!\n");
    exit(1);
  }
  int datasize = 3*width*height;
  if (ftruncate(outfd, strlen(header)+datasize)) {
    printf("Error: Can't allocate disk space for outfile!\n");
    exit(1);
  }
  unsigned char *outdata_ = mmap(NULL, strlen(header)+datasize, PROT_READ|PROT_WRITE, MAP_SHARED, outfd, 0);
  if (outdata_ == MAP_FAILED) {
    printf("Error: Can't mmap outfile!\n");
    exit(1);
  }
  unsigned char *outdata = outdata_ + strlen(header);
  cl_mem cl_outbuf = clCreateBuffer(context, CL_MEM_WRITE_ONLY|CL_MEM_ALLOC_HOST_PTR, datasize, NULL, NULL);
  if (cl_outbuf == NULL) {
    printf("Error: Failed to allocate OpenCL output buffer!\n");
    return 1;
  }
  err = clSetKernelArg(kern, 1, sizeof(cl_outbuf), &cl_outbuf);
  if (err) {
    printf("Error: Can't set kernel arg 1!\n");
    return 1;
  }
  
  
  // ######  Everything is ready, run it!  ######
  printf("data is also ready, starting execution...\n");
  err = clEnqueueNDRangeKernel(commands, kern, 1, NULL, &number_of_runs, NULL, 0, NULL, NULL);
  if (err) {
    printf("Error: Failed to execute kernel!\n");
    return 1;
  }
  printf("waiting for execution to finish...\n");
  err = clFinish(commands);
  if (err) {
    printf("Error: Failed to wait for command queue to finish! %d\n", err);
    return 1;
  }
  
  
  // ######  Fetch computed data.  ######
  printf("execution has finished, fetching results...\n");
  void *outdata2 = clEnqueueMapBuffer(commands, cl_outbuf, CL_TRUE, CL_MAP_READ, 0, datasize, 0, NULL, NULL, NULL);
  if (outdata2 == NULL) {
    printf("Error: Failed to map output buffer!\n");
    return 1;
  }
  memcpy(outdata, outdata2, datasize);
  
  
  printf("done. Thanks for flying with your trusty GPU!\n");
}