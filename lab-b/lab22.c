#include <stdint.h>
#include <stddef.h>

/*
 * Initialisation
*/

struct futhark_context_config ;
struct futhark_context_config *futhark_context_config_new();
void futhark_context_config_free(struct futhark_context_config *cfg);
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int flag);
struct futhark_context ;
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg);
void futhark_context_free(struct futhark_context *ctx);
int futhark_context_sync(struct futhark_context *ctx);

/*
 * Arrays
*/

struct futhark_i32_1d ;
struct futhark_i8_1d ;
struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx,
                                          int32_t *data, int dim0);
int futhark_free_i32_1d(struct futhark_context *ctx,
                        struct futhark_i32_1d *arr);
int futhark_values_i32_1d(struct futhark_context *ctx,
                          struct futhark_i32_1d *arr, int32_t *data);
int64_t *futhark_shape_i32_1d(struct futhark_context *ctx,
                              struct futhark_i32_1d *arr);
struct futhark_i8_1d *futhark_new_i8_1d(struct futhark_context *ctx,
                                        int8_t *data, int dim0);
int futhark_free_i8_1d(struct futhark_context *ctx, struct futhark_i8_1d *arr);
int futhark_values_i8_1d(struct futhark_context *ctx, struct futhark_i8_1d *arr,
                         int8_t *data);
int64_t *futhark_shape_i8_1d(struct futhark_context *ctx,
                             struct futhark_i8_1d *arr);

/*
 * Opaque values
*/

struct futhark_opaque_z37U938627432493791713 ;
int futhark_free_opaque_z37U938627432493791713(struct futhark_context *ctx,
                                               struct futhark_opaque_z37U938627432493791713 *obj);

/*
 * Entry points
*/

int futhark_main(struct futhark_context *ctx, int32_t *out0);
int futhark_random_grid(struct futhark_context *ctx,
                        struct futhark_opaque_z37U938627432493791713 **out0,
                        int32_t in0, int32_t in1, int32_t in2);

/*
 * Miscellaneous
*/

void futhark_debugging_report(struct futhark_context *ctx);
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
/* Crash and burn. */

#include <stdarg.h>

static const char *fut_progname;

static void panic(int eval, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
        fprintf(stderr, "%s: ", fut_progname);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
        exit(eval);
}

/* Some simple utilities for wall-clock timing.

   The function get_wall_time() returns the wall time in microseconds
   (with an unspecified offset).
*/

#ifdef _WIN32

#include <windows.h>

static int64_t get_wall_time() {
  LARGE_INTEGER time,freq;
  assert(QueryPerformanceFrequency(&freq));
  assert(QueryPerformanceCounter(&time));
  return ((double)time.QuadPart / freq.QuadPart) * 1000000;
}

#else
/* Assuming POSIX */

#include <time.h>
#include <sys/time.h>

static int64_t get_wall_time() {
  struct timeval time;
  assert(gettimeofday(&time,NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

#endif

#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <errno.h>
#include <ctype.h>
#include <errno.h>
#include <getopt.h>
//// Text I/O

struct array_reader {
  char* elems;
  int64_t n_elems_space;
  int64_t elem_size;
  int64_t n_elems_used;
  int64_t *shape;
  int (*elem_reader)(void*);
};

static int peekc() {
  int c = getchar();
  if (c != EOF) {
    ungetc(c,stdin);
  }
  return c;
}

static int next_is_not_constituent() {
  int c = peekc();
  return c == EOF || !isalnum(c);
}

static void skipspaces() {
  int c = getchar();
  if (isspace(c)) {
    skipspaces();
  } else if (c == '-' && peekc() == '-') {
    // Skip to end of line.
    for (; c != '\n' && c != EOF; c = getchar());
    // Next line may have more spaces.
    skipspaces();
  } else if (c != EOF) {
    ungetc(c, stdin);
  }
}

static int read_str_elem(struct array_reader *reader) {
  int ret;
  if (reader->n_elems_used == reader->n_elems_space) {
    reader->n_elems_space *= 2;
    reader->elems = (char*) realloc(reader->elems,
                                    reader->n_elems_space * reader->elem_size);
  }

  ret = reader->elem_reader(reader->elems + reader->n_elems_used * reader->elem_size);

  if (ret == 0) {
    reader->n_elems_used++;
  }

  return ret;
}

static int read_str_array_elems(struct array_reader *reader, int dims) {
  int c;
  int ret;
  int first = 1;
  char *knows_dimsize = (char*) calloc(dims,sizeof(char));
  int cur_dim = dims-1;
  int64_t *elems_read_in_dim = (int64_t*) calloc(dims,sizeof(int64_t));
  while (1) {
    skipspaces();

    c = getchar();
    if (c == ']') {
      if (knows_dimsize[cur_dim]) {
        if (reader->shape[cur_dim] != elems_read_in_dim[cur_dim]) {
          ret = 1;
          break;
        }
      } else {
        knows_dimsize[cur_dim] = 1;
        reader->shape[cur_dim] = elems_read_in_dim[cur_dim];
      }
      if (cur_dim == 0) {
        ret = 0;
        break;
      } else {
        cur_dim--;
        elems_read_in_dim[cur_dim]++;
      }
    } else if (c == ',') {
      skipspaces();
      c = getchar();
      if (c == '[') {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        first = 1;
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else if (cur_dim == dims - 1) {
        ungetc(c, stdin);
        ret = read_str_elem(reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
      } else {
        ret = 1;
        break;
      }
    } else if (c == EOF) {
      ret = 1;
      break;
    } else if (first) {
      if (c == '[') {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else {
        ungetc(c, stdin);
        ret = read_str_elem(reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
        first = 0;
      }
    } else {
      ret = 1;
      break;
    }
  }

  free(knows_dimsize);
  free(elems_read_in_dim);
  return ret;
}

static int read_str_empty_array(const char *type_name, int64_t *shape, int64_t dims) {
  char c;
  if (scanf("empty") == EOF) {
    return 1;
  }

  c = getchar();
  if (c != '(') {
    return 1;
  }

  for (int i = 0; i < dims-1; i++) {
    c = getchar();
    if (c != '[') {
      return 1;
    }
    c = getchar();
    if (c != ']') {
      return 1;
    }
  }

  int n = strlen(type_name);
  for (int i = 0; i < n; i++) {
    c = getchar();
    if (c != type_name[i]) {
      return 1;
    }
  }

  if (getchar() != ')') {
    return 1;
  }

  for (int i = 0; i < dims; i++) {
    shape[i] = 0;
  }

  return 0;
}

static int read_str_array(int64_t elem_size, int (*elem_reader)(void*),
                          const char *type_name,
                          void **data, int64_t *shape, int64_t dims) {
  int ret;
  struct array_reader reader;
  int64_t read_dims = 0;

  while (1) {
    int c;
    skipspaces();
    c = getchar();
    if (c=='[') {
      read_dims++;
    } else {
      if (c != EOF) {
        ungetc(c, stdin);
      }
      break;
    }
  }

  if (read_dims == 0) {
    return read_str_empty_array(type_name, shape, dims);
  }

  if (read_dims != dims) {
    return 1;
  }

  reader.shape = shape;
  reader.n_elems_used = 0;
  reader.elem_size = elem_size;
  reader.n_elems_space = 16;
  reader.elems = (char*) realloc(*data, elem_size*reader.n_elems_space);
  reader.elem_reader = elem_reader;

  ret = read_str_array_elems(&reader, dims);

  *data = reader.elems;

  return ret;
}

/* Makes a copy of numeric literal removing any underscores, and
   length of the literal. */
static int remove_underscores(char* buf) {
  int buf_index = 0;
  char c = getchar();
  while (isxdigit(c) || c == '.' || c == '+' || c == '-' ||
         c == 'x' || c == 'X' ||
         c == 'p' || c == 'P' || /* exponent for hex. floats */
         c == 'e' || c == 'E' || c == '_') {
    if (c == '_') {
      c = getchar();
      continue;
    }
    else {
      buf[buf_index++] = c;
      c = getchar();
    }
  }
  buf[buf_index] = 0;
  ungetc(c, stdin);             /* unget 'i' */
  return buf_index;
}

static int read_str_i8(void* dest) {
  skipspaces();
  /* Some platforms (WINDOWS) does not support scanf %hhd or its
     cousin, %SCNi8.  Read into int first to avoid corrupting
     memory.

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417  */
  int x;
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%i", &x) == 1) {
    *(int8_t*)dest = x;
    scanf("i8");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u8(void* dest) {
  skipspaces();
  /* Some platforms (WINDOWS) does not support scanf %hhd or its
     cousin, %SCNu8.  Read into int first to avoid corrupting
     memory.

     https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417  */
  int x;
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%i", &x) == 1) {
    *(uint8_t*)dest = x;
    scanf("u8");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i16(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi16, (int16_t*)dest) == 1) {
    scanf("i16");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    printf("fail\n");
    return 1;
  }
}

static int read_str_u16(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi16, (int16_t*)dest) == 1) {
    scanf("u16");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi32, (int32_t*)dest) == 1) {
    scanf("i32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi32, (int32_t*)dest) == 1) {
    scanf("u32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_i64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%"SCNi64, (int64_t*)dest) == 1) {
    scanf("i64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_u64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  // FIXME: This is not correct, as SCNu64 only permits decimal
  // literals.  However, SCNi64 does not handle very large numbers
  // correctly (it's really for signed numbers, so that's fair).
  if (sscanf(buf, "%"SCNu64, (uint64_t*)dest) == 1) {
    scanf("u64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_f32(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%f", (float*)dest) == 1) {
    scanf("f32");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_f64(void* dest) {
  skipspaces();
  char buf[128];
  remove_underscores(buf);
  if (sscanf(buf, "%lf", (double*)dest) == 1) {
    scanf("f64");
    return next_is_not_constituent() ? 0 : 1;
  } else {
    return 1;
  }
}

static int read_str_bool(void* dest) {
  /* This is a monstrous hack.  Maybe we should get a proper lexer in here. */
  char b[4];
  skipspaces();
  if (scanf("%4c", b) == 1) {
    if (strncmp(b, "true", 4) == 0) {
      *(char*)dest = 1;
      return 0;
    } else if (strncmp(b, "fals", 4) == 0 && getchar() == 'e') {
      *(char*)dest = 0;
      return 0;
    } else {
      return 1;
    }
  } else {
    return 1;
  }
}

static int write_str_i8(FILE *out, int8_t *src) {
  return fprintf(out, "%hhdi8", *src);
}

static int write_str_u8(FILE *out, uint8_t *src) {
  return fprintf(out, "%hhuu8", *src);
}

static int write_str_i16(FILE *out, int16_t *src) {
  return fprintf(out, "%hdi16", *src);
}

static int write_str_u16(FILE *out, uint16_t *src) {
  return fprintf(out, "%huu16", *src);
}

static int write_str_i32(FILE *out, int32_t *src) {
  return fprintf(out, "%di32", *src);
}

static int write_str_u32(FILE *out, uint32_t *src) {
  return fprintf(out, "%uu32", *src);
}

static int write_str_i64(FILE *out, int64_t *src) {
  return fprintf(out, "%"PRIi64"i64", *src);
}

static int write_str_u64(FILE *out, uint64_t *src) {
  return fprintf(out, "%"PRIu64"u64", *src);
}

static int write_str_f32(FILE *out, float *src) {
  return fprintf(out, "%.6ff32", *src);
}

static int write_str_f64(FILE *out, double *src) {
  return fprintf(out, "%.6ff64", *src);
}

static int write_str_bool(FILE *out, void *src) {
  return fprintf(out, *(char*)src ? "true" : "false");
}

//// Binary I/O

#define BINARY_FORMAT_VERSION 2
#define IS_BIG_ENDIAN (!*(unsigned char *)&(uint16_t){1})

// Reading little-endian byte sequences.  On big-endian hosts, we flip
// the resulting bytes.

static int read_byte(void* dest) {
  int num_elems_read = fread(dest, 1, 1, stdin);
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_2byte(void* dest) {
  uint16_t x;
  int num_elems_read = fread(&x, 2, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x = (x>>8) | (x<<8);
  }
  *(uint16_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_4byte(void* dest) {
  uint32_t x;
  int num_elems_read = fread(&x, 4, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>24)&0xFF) |
      ((x>>8) &0xFF00) |
      ((x<<8) &0xFF0000) |
      ((x<<24)&0xFF000000);
  }
  *(uint32_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int read_le_8byte(void* dest) {
  uint64_t x;
  int num_elems_read = fread(&x, 8, 1, stdin);
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>56)&0xFFull) |
      ((x>>40)&0xFF00ull) |
      ((x>>24)&0xFF0000ull) |
      ((x>>8) &0xFF000000ull) |
      ((x<<8) &0xFF00000000ull) |
      ((x<<24)&0xFF0000000000ull) |
      ((x<<40)&0xFF000000000000ull) |
      ((x<<56)&0xFF00000000000000ull);
  }
  *(uint64_t*)dest = x;
  return num_elems_read == 1 ? 0 : 1;
}

static int write_byte(void* dest) {
  int num_elems_written = fwrite(dest, 1, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_2byte(void* dest) {
  uint16_t x = *(uint16_t*)dest;
  if (IS_BIG_ENDIAN) {
    x = (x>>8) | (x<<8);
  }
  int num_elems_written = fwrite(&x, 2, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_4byte(void* dest) {
  uint32_t x = *(uint32_t*)dest;
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>24)&0xFF) |
      ((x>>8) &0xFF00) |
      ((x<<8) &0xFF0000) |
      ((x<<24)&0xFF000000);
  }
  int num_elems_written = fwrite(&x, 4, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

static int write_le_8byte(void* dest) {
  uint64_t x = *(uint64_t*)dest;
  if (IS_BIG_ENDIAN) {
    x =
      ((x>>56)&0xFFull) |
      ((x>>40)&0xFF00ull) |
      ((x>>24)&0xFF0000ull) |
      ((x>>8) &0xFF000000ull) |
      ((x<<8) &0xFF00000000ull) |
      ((x<<24)&0xFF0000000000ull) |
      ((x<<40)&0xFF000000000000ull) |
      ((x<<56)&0xFF00000000000000ull);
  }
  int num_elems_written = fwrite(&x, 8, 1, stdin);
  return num_elems_written == 1 ? 0 : 1;
}

//// Types

typedef int (*writer)(FILE*, void*);
typedef int (*reader)(void*);

struct primtype_info_t {
  const char binname[4]; // Used for parsing binary data.
  const char* type_name; // Same name as in Futhark.
  const int size; // in bytes
  const writer write_str; // Write in text format.
  const reader read_str; // Read in text format.
  const writer write_bin; // Write in binary format.
  const reader read_bin; // Read in binary format.
};

static const struct primtype_info_t i8 =
  {.binname = "  i8", .type_name = "i8",   .size = 1,
   .write_str = (writer)write_str_i8, .read_str = (reader)read_str_i8,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};
static const struct primtype_info_t i16 =
  {.binname = " i16", .type_name = "i16",  .size = 2,
   .write_str = (writer)write_str_i16, .read_str = (reader)read_str_i16,
   .write_bin = (writer)write_le_2byte, .read_bin = (reader)read_le_2byte};
static const struct primtype_info_t i32 =
  {.binname = " i32", .type_name = "i32",  .size = 4,
   .write_str = (writer)write_str_i32, .read_str = (reader)read_str_i32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
static const struct primtype_info_t i64 =
  {.binname = " i64", .type_name = "i64",  .size = 8,
   .write_str = (writer)write_str_i64, .read_str = (reader)read_str_i64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
static const struct primtype_info_t u8 =
  {.binname = "  u8", .type_name = "u8",   .size = 1,
   .write_str = (writer)write_str_u8, .read_str = (reader)read_str_u8,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};
static const struct primtype_info_t u16 =
  {.binname = " u16", .type_name = "u16",  .size = 2,
   .write_str = (writer)write_str_u16, .read_str = (reader)read_str_u16,
   .write_bin = (writer)write_le_2byte, .read_bin = (reader)read_le_2byte};
static const struct primtype_info_t u32 =
  {.binname = " u32", .type_name = "u32",  .size = 4,
   .write_str = (writer)write_str_u32, .read_str = (reader)read_str_u32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
static const struct primtype_info_t u64 =
  {.binname = " u64", .type_name = "u64",  .size = 8,
   .write_str = (writer)write_str_u64, .read_str = (reader)read_str_u64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
static const struct primtype_info_t f32 =
  {.binname = " f32", .type_name = "f32",  .size = 4,
   .write_str = (writer)write_str_f32, .read_str = (reader)read_str_f32,
   .write_bin = (writer)write_le_4byte, .read_bin = (reader)read_le_4byte};
static const struct primtype_info_t f64 =
  {.binname = " f64", .type_name = "f64",  .size = 8,
   .write_str = (writer)write_str_f64, .read_str = (reader)read_str_f64,
   .write_bin = (writer)write_le_8byte, .read_bin = (reader)read_le_8byte};
static const struct primtype_info_t bool =
  {.binname = "bool", .type_name = "bool", .size = 1,
   .write_str = (writer)write_str_bool, .read_str = (reader)read_str_bool,
   .write_bin = (writer)write_byte, .read_bin = (reader)read_byte};

static const struct primtype_info_t* primtypes[] = {
  &i8, &i16, &i32, &i64,
  &u8, &u16, &u32, &u64,
  &f32, &f64,
  &bool,
  NULL // NULL-terminated
};

// General value interface.  All endian business taken care of at
// lower layers.

static int read_is_binary() {
  skipspaces();
  int c = getchar();
  if (c == 'b') {
    int8_t bin_version;
    int ret = read_byte(&bin_version);

    if (ret != 0) { panic(1, "binary-input: could not read version.\n"); }

    if (bin_version != BINARY_FORMAT_VERSION) {
      panic(1, "binary-input: File uses version %i, but I only understand version %i.\n",
            bin_version, BINARY_FORMAT_VERSION);
    }

    return 1;
  }
  ungetc(c, stdin);
  return 0;
}

static const struct primtype_info_t* read_bin_read_type_enum() {
  char read_binname[4];

  int num_matched = scanf("%4c", read_binname);
  if (num_matched != 1) { panic(1, "binary-input: Couldn't read element type.\n"); }

  const struct primtype_info_t **type = primtypes;

  for (; *type != NULL; type++) {
    // I compare the 4 characters manually instead of using strncmp because
    // this allows any value to be used, also NULL bytes
    if (memcmp(read_binname, (*type)->binname, 4) == 0) {
      return *type;
    }
  }
  panic(1, "binary-input: Did not recognize the type '%s'.\n", read_binname);
  return NULL;
}

static void read_bin_ensure_scalar(const struct primtype_info_t *expected_type) {
  int8_t bin_dims;
  int ret = read_byte(&bin_dims);
  if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != 0) {
    panic(1, "binary-input: Expected scalar (0 dimensions), but got array with %i dimensions.\n",
          bin_dims);
  }

  const struct primtype_info_t *bin_type = read_bin_read_type_enum();
  if (bin_type != expected_type) {
    panic(1, "binary-input: Expected scalar of type %s but got scalar of type %s.\n",
          expected_type->type_name,
          bin_type->type_name);
  }
}

//// High-level interface

static int read_bin_array(const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  int ret;

  int8_t bin_dims;
  ret = read_byte(&bin_dims);
  if (ret != 0) { panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != dims) {
    panic(1, "binary-input: Expected %i dimensions, but got array with %i dimensions.\n",
          dims, bin_dims);
  }

  const struct primtype_info_t *bin_primtype = read_bin_read_type_enum();
  if (expected_type != bin_primtype) {
    panic(1, "binary-input: Expected %iD-array with element type '%s' but got %iD-array with element type '%s'.\n",
          dims, expected_type->type_name, dims, bin_primtype->type_name);
  }

  uint64_t elem_count = 1;
  for (int i=0; i<dims; i++) {
    uint64_t bin_shape;
    ret = read_le_8byte(&bin_shape);
    if (ret != 0) { panic(1, "binary-input: Couldn't read size for dimension %i of array.\n", i); }
    elem_count *= bin_shape;
    shape[i] = (int64_t) bin_shape;
  }

  size_t elem_size = expected_type->size;
  void* tmp = realloc(*data, elem_count * elem_size);
  if (tmp == NULL) {
    panic(1, "binary-input: Failed to allocate array of size %i.\n",
          elem_count * elem_size);
  }
  *data = tmp;

  size_t num_elems_read = fread(*data, elem_size, elem_count, stdin);
  if (num_elems_read != elem_count) {
    panic(1, "binary-input: tried to read %i elements of an array, but only got %i elements.\n",
          elem_count, num_elems_read);
  }

  // If we're on big endian platform we must change all multibyte elements
  // from using little endian to big endian
  if (IS_BIG_ENDIAN && elem_size != 1) {
    char* elems = (char*) *data;
    for (uint64_t i=0; i<elem_count; i++) {
      char* elem = elems+(i*elem_size);
      for (unsigned int j=0; j<elem_size/2; j++) {
        char head = elem[j];
        int tail_index = elem_size-1-j;
        elem[j] = elem[tail_index];
        elem[tail_index] = head;
      }
    }
  }

  return 0;
}

static int read_array(const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  if (!read_is_binary()) {
    return read_str_array(expected_type->size, (reader)expected_type->read_str, expected_type->type_name, data, shape, dims);
  } else {
    return read_bin_array(expected_type, data, shape, dims);
  }
}

static int write_str_array(FILE *out, const struct primtype_info_t *elem_type, unsigned char *data, int64_t *shape, int8_t rank) {
  if (rank==0) {
    elem_type->write_str(out, (void*)data);
  } else {
    int64_t len = shape[0];
    int64_t slice_size = 1;

    int64_t elem_size = elem_type->size;
    for (int64_t i = 1; i < rank; i++) {
      slice_size *= shape[i];
    }

    if (len*slice_size == 0) {
      printf("empty(");
      for (int64_t i = 1; i < rank; i++) {
        printf("[]");
      }
      printf("%s", elem_type->type_name);
      printf(")");
    } else if (rank==1) {
      putchar('[');
      for (int64_t i = 0; i < len; i++) {
        elem_type->write_str(out, (void*) (data + i * elem_size));
        if (i != len-1) {
          printf(", ");
        }
      }
      putchar(']');
    } else {
      putchar('[');
      for (int64_t i = 0; i < len; i++) {
        write_str_array(out, elem_type, data + i * slice_size * elem_size, shape+1, rank-1);
        if (i != len-1) {
          printf(", ");
        }
      }
      putchar(']');
    }
  }
  return 0;
}

static int write_bin_array(FILE *out, const struct primtype_info_t *elem_type, unsigned char *data, int64_t *shape, int8_t rank) {
  int64_t num_elems = 1;
  for (int64_t i = 0; i < rank; i++) {
    num_elems *= shape[i];
  }

  fputc('b', out);
  fputc((char)BINARY_FORMAT_VERSION, out);
  fwrite(&rank, sizeof(int8_t), 1, out);
  fputs(elem_type->binname, out);
  fwrite(shape, sizeof(int64_t), rank, out);

  if (IS_BIG_ENDIAN) {
    for (int64_t i = 0; i < num_elems; i++) {
      unsigned char *elem = data+i*elem_type->size;
      for (int64_t j = 0; j < elem_type->size; j++) {
        fwrite(&elem[elem_type->size-j], 1, 1, out);
      }
    }
  } else {
    fwrite(data, elem_type->size, num_elems, out);
  }

  return 0;
}

static int write_array(FILE *out, int write_binary,
                       const struct primtype_info_t *elem_type, void *data, int64_t *shape, int8_t rank) {
  if (write_binary) {
    return write_bin_array(out, elem_type, data, shape, rank);
  } else {
    return write_str_array(out, elem_type, data, shape, rank);
  }
}

static int read_scalar(const struct primtype_info_t *expected_type, void *dest) {
  if (!read_is_binary()) {
    return expected_type->read_str(dest);
  } else {
    read_bin_ensure_scalar(expected_type);
    return expected_type->read_bin(dest);
  }
}

static int write_scalar(FILE *out, int write_binary, const struct primtype_info_t *type, void *src) {
  if (write_binary) {
    return write_bin_array(out, type, src, NULL, 0);
  } else {
    return type->write_str(out, src);
  }
}

static int binary_output = 0;
static FILE *runtime_file;
static int perform_warmup = 0;
static int num_runs = 1;
static const char *entry_point = "main";
int parse_options(struct futhark_context_config *cfg, int argc,
                  char *const argv[])
{
    int ch;
    static struct option long_options[] = {{"write-runtime-to",
                                            required_argument, NULL, 1},
                                           {"runs", required_argument, NULL, 2},
                                           {"debugging", no_argument, NULL, 3},
                                           {"entry-point", required_argument,
                                            NULL, 4}, {"binary-output",
                                                       no_argument, NULL, 5},
                                           {0, 0, 0, 0}};
    
    while ((ch = getopt_long(argc, argv, ":t:r:De:b", long_options, NULL)) !=
           -1) {
        if (ch == 1 || ch == 't') {
            runtime_file = fopen(optarg, "w");
            if (runtime_file == NULL)
                panic(1, "Cannot open %s: %s\n", optarg, strerror(errno));
        }
        if (ch == 2 || ch == 'r') {
            num_runs = atoi(optarg);
            perform_warmup = 1;
            if (num_runs <= 0)
                panic(1, "Need a positive number of runs, not %s\n", optarg);
        }
        if (ch == 3 || ch == 'D')
            futhark_context_config_set_debugging(cfg, 1);
        if (ch == 4 || ch == 'e')
            entry_point = optarg;
        if (ch == 5 || ch == 'b')
            binary_output = 1;
        if (ch == ':')
            panic(-1, "Missing argument for option %s\n", argv[optind - 1]);
        if (ch == '?')
            panic(-1, "Unknown option %s\n", argv[optind - 1]);
    }
    return optind;
}
static void futrts_cli_entry_main(struct futhark_context *ctx)
{
    int64_t t_start, t_end;
    int time_runs;
    int32_t result_8863;
    
    if (perform_warmup) {
        time_runs = 0;
        assert(futhark_context_sync(ctx) == 0);
        t_start = get_wall_time();
        assert(futhark_main(ctx, &result_8863) == 0);
        assert(futhark_context_sync(ctx) == 0);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
        ;
    }
    time_runs = 1;
    /* Proper run. */
    for (int run = 0; run < num_runs; run++) {
        assert(futhark_context_sync(ctx) == 0);
        t_start = get_wall_time();
        assert(futhark_main(ctx, &result_8863) == 0);
        assert(futhark_context_sync(ctx) == 0);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
        if (run < num_runs - 1) {
            ;
        }
    }
    write_scalar(stdout, binary_output, &i32, &result_8863);
    printf("\n");
    ;
}
static void futrts_cli_entry_random_grid(struct futhark_context *ctx)
{
    int64_t t_start, t_end;
    int time_runs;
    int32_t read_value_8865;
    
    if (read_scalar(&i32, &read_value_8865) != 0)
        panic(1, "Error when reading input #%d of type %s (errno: %s).\n", 0,
              i32.type_name, strerror(errno));
    
    int32_t read_value_8866;
    
    if (read_scalar(&i32, &read_value_8866) != 0)
        panic(1, "Error when reading input #%d of type %s (errno: %s).\n", 1,
              i32.type_name, strerror(errno));
    
    int32_t read_value_8867;
    
    if (read_scalar(&i32, &read_value_8867) != 0)
        panic(1, "Error when reading input #%d of type %s (errno: %s).\n", 2,
              i32.type_name, strerror(errno));
    
    struct futhark_opaque_z37U938627432493791713 *result_8868;
    
    if (perform_warmup) {
        time_runs = 0;
        ;
        ;
        ;
        assert(futhark_context_sync(ctx) == 0);
        t_start = get_wall_time();
        assert(futhark_random_grid(ctx, &result_8868, read_value_8865,
                                   read_value_8866, read_value_8867) == 0);
        assert(futhark_context_sync(ctx) == 0);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
        ;
        ;
        ;
        assert(futhark_free_opaque_z37U938627432493791713(ctx, result_8868) ==
            0);
    }
    time_runs = 1;
    /* Proper run. */
    for (int run = 0; run < num_runs; run++) {
        ;
        ;
        ;
        assert(futhark_context_sync(ctx) == 0);
        t_start = get_wall_time();
        assert(futhark_random_grid(ctx, &result_8868, read_value_8865,
                                   read_value_8866, read_value_8867) == 0);
        assert(futhark_context_sync(ctx) == 0);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL)
            fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
        ;
        ;
        ;
        if (run < num_runs - 1) {
            assert(futhark_free_opaque_z37U938627432493791713(ctx,
                                                              result_8868) ==
                0);
        }
    }
    ;
    ;
    ;
    printf("#<opaque %s>", "[](u32, i8)");
    printf("\n");
    assert(futhark_free_opaque_z37U938627432493791713(ctx, result_8868) == 0);
}
typedef void entry_point_fun(struct futhark_context *);
struct entry_point_entry {
    const char *name;
    entry_point_fun *fun;
} ;
int main(int argc, char **argv)
{
    fut_progname = argv[0];
    
    struct entry_point_entry entry_points[] = {{.name ="main", .fun =
                                                futrts_cli_entry_main}, {.name =
                                                                         "random_grid",
                                                                         .fun =
                                                                         futrts_cli_entry_random_grid}};
    struct futhark_context_config *cfg = futhark_context_config_new();
    
    assert(cfg != NULL);
    
    int parsed_options = parse_options(cfg, argc, argv);
    
    argc -= parsed_options;
    argv += parsed_options;
    if (argc != 0)
        panic(1, "Excess non-option: %s\n", argv[0]);
    
    struct futhark_context *ctx = futhark_context_new(cfg);
    
    assert(ctx != NULL);
    
    int num_entry_points = sizeof(entry_points) / sizeof(entry_points[0]);
    entry_point_fun *entry_point_fun = NULL;
    
    for (int i = 0; i < num_entry_points; i++) {
        if (strcmp(entry_points[i].name, entry_point) == 0) {
            entry_point_fun = entry_points[i].fun;
            break;
        }
    }
    if (entry_point_fun == NULL) {
        fprintf(stderr,
                "No entry point '%s'.  Select another with --entry-point.  Options are:\n",
                entry_point);
        for (int i = 0; i < num_entry_points; i++)
            fprintf(stderr, "%s\n", entry_points[i].name);
        return 1;
    }
    entry_point_fun(ctx);
    if (runtime_file != NULL)
        fclose(runtime_file);
    futhark_debugging_report(ctx);
    futhark_context_free(ctx);
    futhark_context_config_free(cfg);
    return 0;
}
#ifdef _MSC_VER
#define inline __inline
#endif
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>
#include <ctype.h>
#include <errno.h>
#include <assert.h>
struct memblock {
    int *references;
    char *mem;
    int64_t size;
    const char *desc;
} ;
struct futhark_context_config {
    int debugging;
} ;
struct futhark_context_config *futhark_context_config_new()
{
    struct futhark_context_config *cfg =
                                  malloc(sizeof(struct futhark_context_config));
    
    if (cfg == NULL)
        return NULL;
    cfg->debugging = 0;
    return cfg;
}
void futhark_context_config_free(struct futhark_context_config *cfg)
{
    free(cfg);
}
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int detail)
{
    cfg->debugging = detail;
}
struct futhark_context {
    int detail_memory;
    int debugging;
    int64_t peak_mem_usage_default;
    int64_t cur_mem_usage_default;
} ;
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg)
{
    struct futhark_context *ctx = malloc(sizeof(struct futhark_context));
    
    if (ctx == NULL)
        return NULL;
    ctx->detail_memory = cfg->debugging;
    ctx->debugging = cfg->debugging;
    ctx->peak_mem_usage_default = 0;
    ctx->cur_mem_usage_default = 0;
    return ctx;
}
void futhark_context_free(struct futhark_context *ctx)
{
    free(ctx);
}
int futhark_context_sync(struct futhark_context *ctx)
{
    ctx = ctx;
    return 0;
}
static void memblock_unref(struct futhark_context *ctx, struct memblock *block,
                           const char *desc)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (ctx->detail_memory)
            fprintf(stderr,
                    "Unreferencing block %s (allocated as %s) in %s: %d references remaining.\n",
                    desc, block->desc, "default space", *block->references);
        if (*block->references == 0) {
            ctx->cur_mem_usage_default -= block->size;
            free(block->mem);
            free(block->references);
            block->references = NULL;
            if (ctx->detail_memory)
                fprintf(stderr,
                        "%lld bytes freed (now allocated: %lld bytes)\n",
                        (long long) block->size,
                        (long long) ctx->cur_mem_usage_default);
        }
    }
}
static void memblock_alloc(struct futhark_context *ctx, struct memblock *block,
                           int64_t size, const char *desc)
{
    if (size < 0)
        panic(1, "Negative allocation of %lld bytes attempted for %s in %s.\n",
              (long long) size, desc, "default space",
              ctx->cur_mem_usage_default);
    memblock_unref(ctx, block, desc);
    block->mem = (char *) malloc(size);
    block->references = (int *) malloc(sizeof(int));
    *block->references = 1;
    block->size = size;
    block->desc = desc;
    ctx->cur_mem_usage_default += size;
    if (ctx->detail_memory)
        fprintf(stderr,
                "Allocated %lld bytes for %s in %s (now allocated: %lld bytes)",
                (long long) size, desc, "default space",
                (long long) ctx->cur_mem_usage_default);
    if (ctx->cur_mem_usage_default > ctx->peak_mem_usage_default) {
        ctx->peak_mem_usage_default = ctx->cur_mem_usage_default;
        if (ctx->detail_memory)
            fprintf(stderr, " (new peak).\n");
    } else if (ctx->detail_memory)
        fprintf(stderr, ".\n");
}
static void memblock_set(struct futhark_context *ctx, struct memblock *lhs,
                         struct memblock *rhs, const char *lhs_desc)
{
    memblock_unref(ctx, lhs, lhs_desc);
    (*rhs->references)++;
    *lhs = *rhs;
}
void futhark_debugging_report(struct futhark_context *ctx)
{
    if (ctx->detail_memory) {
        fprintf(stderr, "Peak memory usage for default space: %lld bytes.\n",
                (long long) ctx->peak_mem_usage_default);
    }
    if (ctx->debugging) { }
}
struct futrts_int64_t_mem_int32_t_int64_t_mem {
    int64_t v0;
    struct memblock v1;
    int32_t v2;
    int64_t v3;
    struct memblock v4;
} ;
struct futrts_int32_t {
    int32_t v0;
} ;
static struct futrts_int32_t futrts_main(struct futhark_context *ctx);
static struct futrts_int64_t_mem_int32_t_int64_t_mem
futrts_random_grid(struct futhark_context *ctx, int32_t seed_8765,
                   int32_t w_8766, int32_t h_8767);
static inline int8_t add8(int8_t x, int8_t y)
{
    return x + y;
}
static inline int16_t add16(int16_t x, int16_t y)
{
    return x + y;
}
static inline int32_t add32(int32_t x, int32_t y)
{
    return x + y;
}
static inline int64_t add64(int64_t x, int64_t y)
{
    return x + y;
}
static inline int8_t sub8(int8_t x, int8_t y)
{
    return x - y;
}
static inline int16_t sub16(int16_t x, int16_t y)
{
    return x - y;
}
static inline int32_t sub32(int32_t x, int32_t y)
{
    return x - y;
}
static inline int64_t sub64(int64_t x, int64_t y)
{
    return x - y;
}
static inline int8_t mul8(int8_t x, int8_t y)
{
    return x * y;
}
static inline int16_t mul16(int16_t x, int16_t y)
{
    return x * y;
}
static inline int32_t mul32(int32_t x, int32_t y)
{
    return x * y;
}
static inline int64_t mul64(int64_t x, int64_t y)
{
    return x * y;
}
static inline uint8_t udiv8(uint8_t x, uint8_t y)
{
    return x / y;
}
static inline uint16_t udiv16(uint16_t x, uint16_t y)
{
    return x / y;
}
static inline uint32_t udiv32(uint32_t x, uint32_t y)
{
    return x / y;
}
static inline uint64_t udiv64(uint64_t x, uint64_t y)
{
    return x / y;
}
static inline uint8_t umod8(uint8_t x, uint8_t y)
{
    return x % y;
}
static inline uint16_t umod16(uint16_t x, uint16_t y)
{
    return x % y;
}
static inline uint32_t umod32(uint32_t x, uint32_t y)
{
    return x % y;
}
static inline uint64_t umod64(uint64_t x, uint64_t y)
{
    return x % y;
}
static inline int8_t sdiv8(int8_t x, int8_t y)
{
    int8_t q = x / y;
    int8_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int16_t sdiv16(int16_t x, int16_t y)
{
    int16_t q = x / y;
    int16_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int32_t sdiv32(int32_t x, int32_t y)
{
    int32_t q = x / y;
    int32_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int64_t sdiv64(int64_t x, int64_t y)
{
    int64_t q = x / y;
    int64_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int8_t smod8(int8_t x, int8_t y)
{
    int8_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int16_t smod16(int16_t x, int16_t y)
{
    int16_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int32_t smod32(int32_t x, int32_t y)
{
    int32_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int64_t smod64(int64_t x, int64_t y)
{
    int64_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int8_t squot8(int8_t x, int8_t y)
{
    return x / y;
}
static inline int16_t squot16(int16_t x, int16_t y)
{
    return x / y;
}
static inline int32_t squot32(int32_t x, int32_t y)
{
    return x / y;
}
static inline int64_t squot64(int64_t x, int64_t y)
{
    return x / y;
}
static inline int8_t srem8(int8_t x, int8_t y)
{
    return x % y;
}
static inline int16_t srem16(int16_t x, int16_t y)
{
    return x % y;
}
static inline int32_t srem32(int32_t x, int32_t y)
{
    return x % y;
}
static inline int64_t srem64(int64_t x, int64_t y)
{
    return x % y;
}
static inline int8_t smin8(int8_t x, int8_t y)
{
    return x < y ? x : y;
}
static inline int16_t smin16(int16_t x, int16_t y)
{
    return x < y ? x : y;
}
static inline int32_t smin32(int32_t x, int32_t y)
{
    return x < y ? x : y;
}
static inline int64_t smin64(int64_t x, int64_t y)
{
    return x < y ? x : y;
}
static inline uint8_t umin8(uint8_t x, uint8_t y)
{
    return x < y ? x : y;
}
static inline uint16_t umin16(uint16_t x, uint16_t y)
{
    return x < y ? x : y;
}
static inline uint32_t umin32(uint32_t x, uint32_t y)
{
    return x < y ? x : y;
}
static inline uint64_t umin64(uint64_t x, uint64_t y)
{
    return x < y ? x : y;
}
static inline int8_t smax8(int8_t x, int8_t y)
{
    return x < y ? y : x;
}
static inline int16_t smax16(int16_t x, int16_t y)
{
    return x < y ? y : x;
}
static inline int32_t smax32(int32_t x, int32_t y)
{
    return x < y ? y : x;
}
static inline int64_t smax64(int64_t x, int64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t umax8(uint8_t x, uint8_t y)
{
    return x < y ? y : x;
}
static inline uint16_t umax16(uint16_t x, uint16_t y)
{
    return x < y ? y : x;
}
static inline uint32_t umax32(uint32_t x, uint32_t y)
{
    return x < y ? y : x;
}
static inline uint64_t umax64(uint64_t x, uint64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t shl8(uint8_t x, uint8_t y)
{
    return x << y;
}
static inline uint16_t shl16(uint16_t x, uint16_t y)
{
    return x << y;
}
static inline uint32_t shl32(uint32_t x, uint32_t y)
{
    return x << y;
}
static inline uint64_t shl64(uint64_t x, uint64_t y)
{
    return x << y;
}
static inline uint8_t lshr8(uint8_t x, uint8_t y)
{
    return x >> y;
}
static inline uint16_t lshr16(uint16_t x, uint16_t y)
{
    return x >> y;
}
static inline uint32_t lshr32(uint32_t x, uint32_t y)
{
    return x >> y;
}
static inline uint64_t lshr64(uint64_t x, uint64_t y)
{
    return x >> y;
}
static inline int8_t ashr8(int8_t x, int8_t y)
{
    return x >> y;
}
static inline int16_t ashr16(int16_t x, int16_t y)
{
    return x >> y;
}
static inline int32_t ashr32(int32_t x, int32_t y)
{
    return x >> y;
}
static inline int64_t ashr64(int64_t x, int64_t y)
{
    return x >> y;
}
static inline uint8_t and8(uint8_t x, uint8_t y)
{
    return x & y;
}
static inline uint16_t and16(uint16_t x, uint16_t y)
{
    return x & y;
}
static inline uint32_t and32(uint32_t x, uint32_t y)
{
    return x & y;
}
static inline uint64_t and64(uint64_t x, uint64_t y)
{
    return x & y;
}
static inline uint8_t or8(uint8_t x, uint8_t y)
{
    return x | y;
}
static inline uint16_t or16(uint16_t x, uint16_t y)
{
    return x | y;
}
static inline uint32_t or32(uint32_t x, uint32_t y)
{
    return x | y;
}
static inline uint64_t or64(uint64_t x, uint64_t y)
{
    return x | y;
}
static inline uint8_t xor8(uint8_t x, uint8_t y)
{
    return x ^ y;
}
static inline uint16_t xor16(uint16_t x, uint16_t y)
{
    return x ^ y;
}
static inline uint32_t xor32(uint32_t x, uint32_t y)
{
    return x ^ y;
}
static inline uint64_t xor64(uint64_t x, uint64_t y)
{
    return x ^ y;
}
static inline char ult8(uint8_t x, uint8_t y)
{
    return x < y;
}
static inline char ult16(uint16_t x, uint16_t y)
{
    return x < y;
}
static inline char ult32(uint32_t x, uint32_t y)
{
    return x < y;
}
static inline char ult64(uint64_t x, uint64_t y)
{
    return x < y;
}
static inline char ule8(uint8_t x, uint8_t y)
{
    return x <= y;
}
static inline char ule16(uint16_t x, uint16_t y)
{
    return x <= y;
}
static inline char ule32(uint32_t x, uint32_t y)
{
    return x <= y;
}
static inline char ule64(uint64_t x, uint64_t y)
{
    return x <= y;
}
static inline char slt8(int8_t x, int8_t y)
{
    return x < y;
}
static inline char slt16(int16_t x, int16_t y)
{
    return x < y;
}
static inline char slt32(int32_t x, int32_t y)
{
    return x < y;
}
static inline char slt64(int64_t x, int64_t y)
{
    return x < y;
}
static inline char sle8(int8_t x, int8_t y)
{
    return x <= y;
}
static inline char sle16(int16_t x, int16_t y)
{
    return x <= y;
}
static inline char sle32(int32_t x, int32_t y)
{
    return x <= y;
}
static inline char sle64(int64_t x, int64_t y)
{
    return x <= y;
}
static inline int8_t pow8(int8_t x, int8_t y)
{
    int8_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int16_t pow16(int16_t x, int16_t y)
{
    int16_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int32_t pow32(int32_t x, int32_t y)
{
    int32_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int64_t pow64(int64_t x, int64_t y)
{
    int64_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int8_t sext_i8_i8(int8_t x)
{
    return x;
}
static inline int16_t sext_i8_i16(int8_t x)
{
    return x;
}
static inline int32_t sext_i8_i32(int8_t x)
{
    return x;
}
static inline int64_t sext_i8_i64(int8_t x)
{
    return x;
}
static inline int8_t sext_i16_i8(int16_t x)
{
    return x;
}
static inline int16_t sext_i16_i16(int16_t x)
{
    return x;
}
static inline int32_t sext_i16_i32(int16_t x)
{
    return x;
}
static inline int64_t sext_i16_i64(int16_t x)
{
    return x;
}
static inline int8_t sext_i32_i8(int32_t x)
{
    return x;
}
static inline int16_t sext_i32_i16(int32_t x)
{
    return x;
}
static inline int32_t sext_i32_i32(int32_t x)
{
    return x;
}
static inline int64_t sext_i32_i64(int32_t x)
{
    return x;
}
static inline int8_t sext_i64_i8(int64_t x)
{
    return x;
}
static inline int16_t sext_i64_i16(int64_t x)
{
    return x;
}
static inline int32_t sext_i64_i32(int64_t x)
{
    return x;
}
static inline int64_t sext_i64_i64(int64_t x)
{
    return x;
}
static inline uint8_t zext_i8_i8(uint8_t x)
{
    return x;
}
static inline uint16_t zext_i8_i16(uint8_t x)
{
    return x;
}
static inline uint32_t zext_i8_i32(uint8_t x)
{
    return x;
}
static inline uint64_t zext_i8_i64(uint8_t x)
{
    return x;
}
static inline uint8_t zext_i16_i8(uint16_t x)
{
    return x;
}
static inline uint16_t zext_i16_i16(uint16_t x)
{
    return x;
}
static inline uint32_t zext_i16_i32(uint16_t x)
{
    return x;
}
static inline uint64_t zext_i16_i64(uint16_t x)
{
    return x;
}
static inline uint8_t zext_i32_i8(uint32_t x)
{
    return x;
}
static inline uint16_t zext_i32_i16(uint32_t x)
{
    return x;
}
static inline uint32_t zext_i32_i32(uint32_t x)
{
    return x;
}
static inline uint64_t zext_i32_i64(uint32_t x)
{
    return x;
}
static inline uint8_t zext_i64_i8(uint64_t x)
{
    return x;
}
static inline uint16_t zext_i64_i16(uint64_t x)
{
    return x;
}
static inline uint32_t zext_i64_i32(uint64_t x)
{
    return x;
}
static inline uint64_t zext_i64_i64(uint64_t x)
{
    return x;
}
static inline float fdiv32(float x, float y)
{
    return x / y;
}
static inline float fadd32(float x, float y)
{
    return x + y;
}
static inline float fsub32(float x, float y)
{
    return x - y;
}
static inline float fmul32(float x, float y)
{
    return x * y;
}
static inline float fmin32(float x, float y)
{
    return x < y ? x : y;
}
static inline float fmax32(float x, float y)
{
    return x < y ? y : x;
}
static inline float fpow32(float x, float y)
{
    return pow(x, y);
}
static inline char cmplt32(float x, float y)
{
    return x < y;
}
static inline char cmple32(float x, float y)
{
    return x <= y;
}
static inline float sitofp_i8_f32(int8_t x)
{
    return x;
}
static inline float sitofp_i16_f32(int16_t x)
{
    return x;
}
static inline float sitofp_i32_f32(int32_t x)
{
    return x;
}
static inline float sitofp_i64_f32(int64_t x)
{
    return x;
}
static inline float uitofp_i8_f32(uint8_t x)
{
    return x;
}
static inline float uitofp_i16_f32(uint16_t x)
{
    return x;
}
static inline float uitofp_i32_f32(uint32_t x)
{
    return x;
}
static inline float uitofp_i64_f32(uint64_t x)
{
    return x;
}
static inline int8_t fptosi_f32_i8(float x)
{
    return x;
}
static inline int16_t fptosi_f32_i16(float x)
{
    return x;
}
static inline int32_t fptosi_f32_i32(float x)
{
    return x;
}
static inline int64_t fptosi_f32_i64(float x)
{
    return x;
}
static inline uint8_t fptoui_f32_i8(float x)
{
    return x;
}
static inline uint16_t fptoui_f32_i16(float x)
{
    return x;
}
static inline uint32_t fptoui_f32_i32(float x)
{
    return x;
}
static inline uint64_t fptoui_f32_i64(float x)
{
    return x;
}
static inline double fdiv64(double x, double y)
{
    return x / y;
}
static inline double fadd64(double x, double y)
{
    return x + y;
}
static inline double fsub64(double x, double y)
{
    return x - y;
}
static inline double fmul64(double x, double y)
{
    return x * y;
}
static inline double fmin64(double x, double y)
{
    return x < y ? x : y;
}
static inline double fmax64(double x, double y)
{
    return x < y ? y : x;
}
static inline double fpow64(double x, double y)
{
    return pow(x, y);
}
static inline char cmplt64(double x, double y)
{
    return x < y;
}
static inline char cmple64(double x, double y)
{
    return x <= y;
}
static inline double sitofp_i8_f64(int8_t x)
{
    return x;
}
static inline double sitofp_i16_f64(int16_t x)
{
    return x;
}
static inline double sitofp_i32_f64(int32_t x)
{
    return x;
}
static inline double sitofp_i64_f64(int64_t x)
{
    return x;
}
static inline double uitofp_i8_f64(uint8_t x)
{
    return x;
}
static inline double uitofp_i16_f64(uint16_t x)
{
    return x;
}
static inline double uitofp_i32_f64(uint32_t x)
{
    return x;
}
static inline double uitofp_i64_f64(uint64_t x)
{
    return x;
}
static inline int8_t fptosi_f64_i8(double x)
{
    return x;
}
static inline int16_t fptosi_f64_i16(double x)
{
    return x;
}
static inline int32_t fptosi_f64_i32(double x)
{
    return x;
}
static inline int64_t fptosi_f64_i64(double x)
{
    return x;
}
static inline uint8_t fptoui_f64_i8(double x)
{
    return x;
}
static inline uint16_t fptoui_f64_i16(double x)
{
    return x;
}
static inline uint32_t fptoui_f64_i32(double x)
{
    return x;
}
static inline uint64_t fptoui_f64_i64(double x)
{
    return x;
}
static inline float fpconv_f32_f32(float x)
{
    return x;
}
static inline double fpconv_f32_f64(float x)
{
    return x;
}
static inline float fpconv_f64_f32(double x)
{
    return x;
}
static inline double fpconv_f64_f64(double x)
{
    return x;
}
static inline float futrts_log32(float x)
{
    return log(x);
}
static inline float futrts_sqrt32(float x)
{
    return sqrt(x);
}
static inline float futrts_exp32(float x)
{
    return exp(x);
}
static inline float futrts_cos32(float x)
{
    return cos(x);
}
static inline float futrts_sin32(float x)
{
    return sin(x);
}
static inline float futrts_acos32(float x)
{
    return acos(x);
}
static inline float futrts_asin32(float x)
{
    return asin(x);
}
static inline float futrts_atan32(float x)
{
    return atan(x);
}
static inline float futrts_atan2_32(float x, float y)
{
    return atan2(x, y);
}
static inline char futrts_isnan32(float x)
{
    return isnan(x);
}
static inline char futrts_isinf32(float x)
{
    return isinf(x);
}
static inline int32_t futrts_to_bits32(float x)
{
    union {
        float f;
        int32_t t;
    } p;
    
    p.f = x;
    return p.t;
}
static inline float futrts_from_bits32(int32_t x)
{
    union {
        int32_t f;
        float t;
    } p;
    
    p.f = x;
    return p.t;
}
static inline double futrts_log64(double x)
{
    return log(x);
}
static inline double futrts_sqrt64(double x)
{
    return sqrt(x);
}
static inline double futrts_exp64(double x)
{
    return exp(x);
}
static inline double futrts_cos64(double x)
{
    return cos(x);
}
static inline double futrts_sin64(double x)
{
    return sin(x);
}
static inline double futrts_acos64(double x)
{
    return acos(x);
}
static inline double futrts_asin64(double x)
{
    return asin(x);
}
static inline double futrts_atan64(double x)
{
    return atan(x);
}
static inline double futrts_atan2_64(double x, double y)
{
    return atan2(x, y);
}
static inline char futrts_isnan64(double x)
{
    return isnan(x);
}
static inline char futrts_isinf64(double x)
{
    return isinf(x);
}
static inline int64_t futrts_to_bits64(double x)
{
    union {
        double f;
        int64_t t;
    } p;
    
    p.f = x;
    return p.t;
}
static inline double futrts_from_bits64(int64_t x)
{
    union {
        int64_t f;
        double t;
    } p;
    
    p.f = x;
    return p.t;
}
static struct futrts_int32_t futrts_main(struct futhark_context *ctx)
{
    int32_t scalar_out_8846;
    
    scalar_out_8846 = 2;
    
    struct futrts_int32_t retval_8860;
    
    retval_8860.v0 = scalar_out_8846;
    return retval_8860;
}
static
struct futrts_int64_t_mem_int32_t_int64_t_mem futrts_random_grid(struct futhark_context *ctx,
                                                                 int32_t seed_8765,
                                                                 int32_t w_8766,
                                                                 int32_t h_8767)
{
    int64_t out_memsizze_8848;
    struct memblock out_mem_8847;
    
    out_mem_8847.references = NULL;
    
    int32_t out_arrsizze_8849;
    int64_t out_memsizze_8851;
    struct memblock out_mem_8850;
    
    out_mem_8850.references = NULL;
    
    int32_t arg_8768 = seed_8765 ^ 5461;
    int32_t arg_8769 = 1 ^ arg_8768;
    int32_t arg_8770 = 48271 * arg_8769;
    int32_t arg_8771 = umod32(arg_8770, 2147483647);
    int32_t res_8772 = 48271 * arg_8771;
    int32_t res_8773 = umod32(res_8772, 2147483647);
    char res_8774 = ule32(2147483646, res_8773);
    char res_8775;
    int32_t res_8776;
    char loop_while_8778;
    int32_t rng_8779;
    
    loop_while_8778 = res_8774;
    rng_8779 = res_8773;
    while (loop_while_8778) {
        int32_t res_8781 = 48271 * rng_8779;
        int32_t res_8782 = umod32(res_8781, 2147483647);
        char res_8783 = ule32(2147483646, res_8782);
        char loop_while_tmp_8852 = res_8783;
        int32_t rng_tmp_8853;
        
        rng_tmp_8853 = res_8782;
        loop_while_8778 = loop_while_tmp_8852;
        rng_8779 = rng_tmp_8853;
    }
    res_8775 = loop_while_8778;
    res_8776 = rng_8779;
    
    int32_t arg_8788 = w_8766 * h_8767;
    int64_t binop_x_8834 = sext_i32_i64(arg_8788);
    int64_t bytes_8833 = binop_x_8834 * 4;
    struct memblock mem_8835;
    
    mem_8835.references = NULL;
    memblock_alloc(ctx, &mem_8835, bytes_8833, "mem_8835");
    
    struct memblock mem_8837;
    
    mem_8837.references = NULL;
    memblock_alloc(ctx, &mem_8837, binop_x_8834, "mem_8837");
    
    int32_t discard_8831;
    int32_t acc_8821 = res_8776;
    
    for (int32_t i_8820 = 0; i_8820 < arg_8788; i_8820++) {
        int32_t res_8800 = 48271 * acc_8821;
        int32_t res_8801 = umod32(res_8800, 2147483647);
        char res_8802 = ule32(2147483646, res_8801);
        char res_8803;
        int32_t res_8804;
        int32_t res_8805;
        char loop_while_8806;
        int32_t rng_8807;
        int32_t x_8808;
        
        loop_while_8806 = res_8802;
        rng_8807 = res_8801;
        x_8808 = res_8801;
        while (loop_while_8806) {
            int32_t res_8809 = 48271 * rng_8807;
            int32_t res_8810 = umod32(res_8809, 2147483647);
            char res_8811 = ule32(2147483646, res_8810);
            char loop_while_tmp_8857 = res_8811;
            int32_t rng_tmp_8858 = res_8810;
            int32_t x_tmp_8859;
            
            x_tmp_8859 = res_8810;
            loop_while_8806 = loop_while_tmp_8857;
            rng_8807 = rng_tmp_8858;
            x_8808 = x_tmp_8859;
        }
        res_8803 = loop_while_8806;
        res_8804 = rng_8807;
        res_8805 = x_8808;
        
        int32_t res_8812 = udiv32(res_8805, 357913941);
        int32_t res_8813 = 1 + res_8812;
        int64_t res_8814 = zext_i32_i64(res_8813);
        int8_t res_8815 = sext_i64_i8(res_8814);
        
        *(int32_t *) &mem_8835.mem[i_8820 * 4] = res_8804;
        *(int8_t *) &mem_8837.mem[i_8820] = res_8815;
        
        int32_t row_8829 = *(int32_t *) &mem_8835.mem[i_8820 * 4];
        int32_t acc_tmp_8854 = row_8829;
        
        acc_8821 = acc_tmp_8854;
    }
    discard_8831 = acc_8821;
    
    char assert_arg_8816 = arg_8788 == h_8767;
    char shape_ok_8817;
    
    if (!assert_arg_8816) {
        fprintf(stderr, "Assertion failed at %s: %s\n",
                "LAB22.fut:89:1-93:94 -> LAB22.fut:93:20-93:94",
                "new shape has different number of elements than old shape");
        exit(1);
    }
    out_arrsizze_8849 = h_8767;
    out_memsizze_8848 = bytes_8833;
    memblock_set(ctx, &out_mem_8847, &mem_8835, "mem_8835");
    out_memsizze_8851 = binop_x_8834;
    memblock_set(ctx, &out_mem_8850, &mem_8837, "mem_8837");
    
    struct futrts_int64_t_mem_int32_t_int64_t_mem retval_8861;
    
    retval_8861.v0 = out_memsizze_8848;
    retval_8861.v1.references = NULL;
    memblock_set(ctx, &retval_8861.v1, &out_mem_8847, "out_mem_8847");
    retval_8861.v2 = out_arrsizze_8849;
    retval_8861.v3 = out_memsizze_8851;
    retval_8861.v4.references = NULL;
    memblock_set(ctx, &retval_8861.v4, &out_mem_8850, "out_mem_8850");
    memblock_unref(ctx, &out_mem_8847, "out_mem_8847");
    memblock_unref(ctx, &out_mem_8850, "out_mem_8850");
    memblock_unref(ctx, &mem_8835, "mem_8835");
    memblock_unref(ctx, &mem_8837, "mem_8837");
    return retval_8861;
}
struct futhark_i8_1d {
    struct memblock mem;
    int64_t shape[1];
} ;
struct futhark_i8_1d *futhark_new_i8_1d(struct futhark_context *ctx,
                                        int8_t *data, int dim0)
{
    struct futhark_i8_1d *arr = malloc(sizeof(struct futhark_i8_1d));
    
    if (arr == NULL)
        return NULL;
    arr->mem.references = NULL;
    memblock_alloc(ctx, &arr->mem, dim0 * sizeof(int8_t), "arr->mem");
    memmove(arr->mem.mem + 0, data + 0, dim0 * sizeof(int8_t));
    arr->shape[0] = dim0;
    return arr;
}
int futhark_free_i8_1d(struct futhark_context *ctx, struct futhark_i8_1d *arr)
{
    memblock_unref(ctx, &arr->mem, "arr->mem");
    free(arr);
    return 0;
}
int futhark_values_i8_1d(struct futhark_context *ctx, struct futhark_i8_1d *arr,
                         int8_t *data)
{
    memmove(data + 0, arr->mem.mem + 0, arr->shape[0] * sizeof(int8_t));
    return 0;
}
int64_t *futhark_shape_i8_1d(struct futhark_context *ctx,
                             struct futhark_i8_1d *arr)
{
    return arr->shape;
}
struct futhark_i32_1d {
    struct memblock mem;
    int64_t shape[1];
} ;
struct futhark_i32_1d *futhark_new_i32_1d(struct futhark_context *ctx,
                                          int32_t *data, int dim0)
{
    struct futhark_i32_1d *arr = malloc(sizeof(struct futhark_i32_1d));
    
    if (arr == NULL)
        return NULL;
    arr->mem.references = NULL;
    memblock_alloc(ctx, &arr->mem, dim0 * sizeof(int32_t), "arr->mem");
    memmove(arr->mem.mem + 0, data + 0, dim0 * sizeof(int32_t));
    arr->shape[0] = dim0;
    return arr;
}
int futhark_free_i32_1d(struct futhark_context *ctx, struct futhark_i32_1d *arr)
{
    memblock_unref(ctx, &arr->mem, "arr->mem");
    free(arr);
    return 0;
}
int futhark_values_i32_1d(struct futhark_context *ctx,
                          struct futhark_i32_1d *arr, int32_t *data)
{
    memmove(data + 0, arr->mem.mem + 0, arr->shape[0] * sizeof(int32_t));
    return 0;
}
int64_t *futhark_shape_i32_1d(struct futhark_context *ctx,
                              struct futhark_i32_1d *arr)
{
    return arr->shape;
}
struct futhark_opaque_z37U938627432493791713 {
    struct futhark_i32_1d *v0;
    struct futhark_i8_1d *v1;
} ;
int futhark_free_opaque_z37U938627432493791713(struct futhark_context *ctx,
                                               struct futhark_opaque_z37U938627432493791713 *obj)
{
    int ret = 0, tmp;
    
    if ((tmp = futhark_free_i32_1d(ctx, obj->v0)) != 0)
        ret = tmp;
    if ((tmp = futhark_free_i8_1d(ctx, obj->v1)) != 0)
        ret = tmp;
    free(obj);
    return ret;
}
int futhark_main(struct futhark_context *ctx, int32_t *out0)
{
    int32_t scalar_out_8846;
    struct futrts_int32_t ret_8862;
    
    ret_8862 = futrts_main(ctx);
    scalar_out_8846 = ret_8862.v0;
    *out0 = scalar_out_8846;
    return 0;
}
int futhark_random_grid(struct futhark_context *ctx,
                        struct futhark_opaque_z37U938627432493791713 **out0,
                        int32_t in0, int32_t in1, int32_t in2)
{
    int32_t seed_8765;
    int32_t w_8766;
    int32_t h_8767;
    int64_t out_memsizze_8848;
    struct memblock out_mem_8847;
    
    out_mem_8847.references = NULL;
    
    int32_t out_arrsizze_8849;
    int64_t out_memsizze_8851;
    struct memblock out_mem_8850;
    
    out_mem_8850.references = NULL;
    seed_8765 = in0;
    w_8766 = in1;
    h_8767 = in2;
    
    struct futrts_int64_t_mem_int32_t_int64_t_mem ret_8864;
    
    ret_8864 = futrts_random_grid(ctx, seed_8765, w_8766, h_8767);
    out_memsizze_8848 = ret_8864.v0;
    out_mem_8847 = ret_8864.v1;
    out_arrsizze_8849 = ret_8864.v2;
    out_memsizze_8851 = ret_8864.v3;
    out_mem_8850 = ret_8864.v4;
    assert((*out0 =
            malloc(sizeof(struct futhark_opaque_z37U938627432493791713))) !=
        NULL);
    assert(((*out0)->v0 = malloc(sizeof(struct futhark_i32_1d))) != NULL);
    (*out0)->v0->mem = out_mem_8847;
    (*out0)->v0->shape[0] = out_arrsizze_8849;
    assert(((*out0)->v1 = malloc(sizeof(struct futhark_i8_1d))) != NULL);
    (*out0)->v1->mem = out_mem_8850;
    (*out0)->v1->shape[0] = out_arrsizze_8849;
    return 0;
}
