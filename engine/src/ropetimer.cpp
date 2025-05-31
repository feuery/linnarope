#include <time.h>
#include <ropetimer.h>

int mstimer() {
  timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);

  return (t.tv_sec * 1000 + t.tv_nsec / 1000000);
}
