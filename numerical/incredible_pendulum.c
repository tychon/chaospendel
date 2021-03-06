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
  if (argc != 12) {
    fprintf(stderr, "wrong invocation: want cpu/gpu, width, height, off, len, outpath, csvoutpath!\n");
    exit(1);
  }
  int use_gpu;
  if (!strcmp(argv[1], "cpu")) {
    use_gpu = 0;
  } else if (!strcmp(argv[1], "gpu")) {
    use_gpu = 1;
  } else {
    printf("I only know cpu and gpu!\n");
    exit(1);
  }
  int width = atoi(argv[2]);
  int height = atoi(argv[3]);
  int off = atoi(argv[4]);
  size_t len = atoi(argv[5]);
  char *outpath = argv[6];
  char *csvoutpath = argv[7];
  double minphi1 = strtod(argv[8], NULL);
  double maxphi1 = strtod(argv[9], NULL);
  double minphi2 = strtod(argv[10], NULL);
  double maxphi2 = strtod(argv[11], NULL);
  
  FILE *csvout = fopen(csvoutpath, "w");
  if (csvout == NULL) {
    perror("can't open csvout file");
    return 1;
  }
  
  
  // ######  Prepare the OpenCL environment and our kernel.  ######
  printf("preparing the OpenCL environment and the kernel...\n");
  cl_platform_id platforms[16];
  cl_uint num_platforms;
  err = clGetPlatformIDs(15, platforms, &num_platforms);
  if (err || num_platforms == 0) {
    printf("Can't locate platform!");
    return 1;
  }
  cl_platform_id platform = platforms[0];
  printf("%i platforms found.\n", (int)num_platforms);
  cl_device_id device_id;
  err = clGetDeviceIDs(platform, use_gpu?CL_DEVICE_TYPE_GPU:CL_DEVICE_TYPE_CPU, 1, &device_id, NULL);
  if (err) {
    printf("Error: Failed to locate a compute device!\n");
    return 1;
  }
  printf("Yay! Seems as if you've got a working compute device!\n");
  
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
  
  err = clBuildProgram(prog, 0, NULL, "-cl-unsafe-math-optimizations", NULL, NULL);
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
  //size_t number_of_all_runs = width * height;
  cl_float *inputdata;
  err = posix_memalign((void**)&inputdata, 2*sizeof(cl_float), 2*sizeof(cl_float)*len);
  if (err) {
    printf("Error allocating aligned memory.\n");
    exit(1);
  }
  cl_float *inputdata_ = inputdata;
  if (inputdata == NULL) {
    printf("Error: Failed to malloc input buffer!\n");
    return 1;
  }
  int y = off/width;
  int x = off%width;
  int n=0;
  for (; y<height; y++) {
    for (; x<width; x++) {
      cl_float phi1 = minphi1+(maxphi1-minphi1)*(1-y/(cl_float)height);
      *(inputdata_++) = phi1;
      cl_float phi2 = minphi2+(maxphi2-minphi2)*(x/(cl_float)width);
      *(inputdata_++) = phi2;
      n++;
      if (n == len) goto loopend;
    }
    x=0;
  }
loopend: ;
  cl_mem cl_inbuf = clCreateBuffer(context, CL_MEM_READ_ONLY|CL_MEM_USE_HOST_PTR, 2*sizeof(cl_float)*len, inputdata, NULL);
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
  int full_datasize = 3*width*height;
  int part_datasize = 3*len;
  off_t data_offset = 3*off;
  if (ftruncate(outfd, strlen(header)+full_datasize)) {
    printf("Error: Can't allocate disk space for outfile!\n");
    exit(1);
  }
  unsigned char *outdata_ = mmap(NULL, strlen(header)+full_datasize, PROT_READ|PROT_WRITE, MAP_SHARED, outfd, 0);
  if (outdata_ == MAP_FAILED) {
    printf("Error: Can't mmap outfile!\n");
    exit(1);
  }
  unsigned char *outdata = outdata_ + strlen(header) + data_offset;
  cl_mem cl_outbuf = clCreateBuffer(context, CL_MEM_WRITE_ONLY|CL_MEM_ALLOC_HOST_PTR, part_datasize, NULL, NULL);
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
  printf("data is also ready, starting execution of %d jobs...\n", (int)len);
  err = clEnqueueNDRangeKernel(commands, kern, 1, NULL, &len, NULL, 0, NULL, NULL);
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
  void *outdata2 = clEnqueueMapBuffer(commands, cl_outbuf, CL_TRUE, CL_MAP_READ, 0, part_datasize, 0, NULL, NULL, NULL);
  if (outdata2 == NULL) {
    printf("Error: Failed to map output buffer!\n");
    return 1;
  }
  memcpy(outdata, outdata2, part_datasize);
  
  for (int i=0; i<len; i++) {
    unsigned char *pixel = outdata2+3*i;
    double phi1 = inputdata[2*i+0];
    double phi2 = inputdata[2*i+1];
    fprintf(csvout, "%f,%f,%hhu,%hhu\n", phi1, phi2, *(unsigned char*)(pixel+1), *(unsigned char*)(pixel+2));
  }
  
  
  printf("done. Thanks for flying with your trusty GPU!\n");
}
