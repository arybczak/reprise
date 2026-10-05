#define _XOPEN_SOURCE 700

#include <time.h>

// Sleep until a time of the monotonic clock, in seconds, which is the clock
// of GHC's getMonotonicTime. A signal ends the sleep early, so that the
// thread can be killed.
void reprise_sleep_until(double seconds)
{
  struct timespec until;
  until.tv_sec = (time_t)seconds;
  until.tv_nsec = (long)((seconds - (double)until.tv_sec) * 1e9);
  clock_nanosleep(CLOCK_MONOTONIC, TIMER_ABSTIME, &until, NULL);
}
