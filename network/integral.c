typedef struct {
  size_t size;
  double min_signi_val;
  int values_under_min;
  
  off_t first_valid;
  double values_sum;
  double values[];
} sliding_window_data;

void swin_add(sliding_window_data *sw, double value) {
  
}

