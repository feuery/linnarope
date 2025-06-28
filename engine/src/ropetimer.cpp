#include <cassert>
#include <time.h>
#include <ropetimer.h>

long mstimer() {
  timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);

  long result =  (t.tv_sec * 1000 + t.tv_nsec / 1000000);
  assert(result >= 0);

  return result;
}
