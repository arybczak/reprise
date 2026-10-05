#include <math.h>
#include <stdint.h>
#include <string.h>

#include "pocketfft/pocketfft.h"

// The magnitudes of the spectrum of one channel of 16-bit samples, which
// interleave the channels: one for each of the length / 2 + 1 bins of the
// plan's transform, over the number of samples, as in ncmpcpp. The samples
// are multiplied by the window, which has their number, and padded with
// silence to the length of the transform, which must be even and at least
// the number of samples. The work buffer has the length of the transform.
// Returns 0, or -1 if pocketfft couldn't allocate its buffers.
int reprise_spectrum(rfft_plan plan, const unsigned char *pcm, const double *window,
                     size_t samples, size_t channels, size_t channel, double *work,
                     double *out)
{
  const size_t length = rfft_length(plan);
  for (size_t i = 0; i < samples; ++i) {
    // The bytes may not be aligned for a 16-bit read.
    int16_t sample;
    memcpy(&sample, pcm + sizeof sample * (i * channels + channel), sizeof sample);
    work[i] = window[i] * sample / -(double)INT16_MIN;
  }
  memset(work + samples, 0, (length - samples) * sizeof *work);
  if (rfft_forward(plan, work, 1) != 0)
    return -1;
  // pocketfft gives the real part of the first bin, the real and the
  // imaginary parts of the next ones, and the real part of the last one.
  const double scale = 1.0 / samples;
  out[0] = fabs(work[0]) * scale;
  for (size_t k = 1; k < length / 2; ++k)
    out[k] = sqrt(work[2 * k - 1] * work[2 * k - 1] + work[2 * k] * work[2 * k]) * scale;
  out[length / 2] = fabs(work[length - 1]) * scale;
  return 0;
}
