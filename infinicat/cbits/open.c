#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

int open_blocking(const char *path) {
  return open(path, O_RDONLY);
}
