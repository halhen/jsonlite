#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct {
  const char* name; // Name of column
  SEXPTYPE type; // Most complex type fund for column
  bool scalar; // Column only contain data of length 0 or 1
} ColumnInfo;

// Dynamic array
typedef struct {
  ColumnInfo* info;
  R_xlen_t used;
  R_xlen_t capacity;
} ColumnInfoArray;

void init_column_info_array(ColumnInfoArray* a) {
  a->info = NULL;
  a->used = a->capacity = 0;
}

void free_column_info_array(ColumnInfoArray* a) {
  if (a->info)
    free(a->info);
  a->info = NULL;
  a->used = a->capacity = 0;
}

static int column_info_compare(const void* a, const void* b) {
  return strcmp(
    ((const ColumnInfo*)a)->name,
    ((const ColumnInfo*)b)->name
  );
}

void sort_column_info_by_name(ColumnInfoArray* a) {
  qsort(a->info, a->used, sizeof(ColumnInfo), column_info_compare);
}

ColumnInfo* find_column_info(ColumnInfoArray* a, const char* name) {
  for (R_xlen_t i = 0; i < a->used; i ++) {
    if (strcmp(a->info[i].name, name) == 0) {
      return a->info + i;
    }
  }

  // Column not found, add a new one
  ColumnInfo new_col = {
    .name = name,
    .type = LGLSXP,
    .scalar = true
  };

  if (a->used >= a->capacity) {
    a->capacity = a->capacity * 2 + 16;
    a->info = realloc(a->info, a->capacity * sizeof(ColumnInfo));
  }

  a->info[a->used++] = new_col;
  return (a->info + (a->used - 1));
}


// returns true if we can simplify data
bool generate_column_info(ColumnInfoArray* column_info, SEXP original_data) {
  R_xlen_t nrow = Rf_length(original_data);

  for (R_xlen_t i = 0; i < nrow; i ++) {
    SEXP row = VECTOR_ELT(original_data, i);
    SEXP rownames = getAttrib(row, R_NamesSymbol);
    R_xlen_t n_rowcols = Rf_length(rownames);

    for (R_xlen_t j = 0; j < n_rowcols; j++) {
      ColumnInfo* pinfo = find_column_info(column_info, CHAR(STRING_ELT(rownames, j)));
      // Assign type. Must be one of lgl, int, real, str, or vec. No point in checking deeper than VECSXP
      if (pinfo->type < VECSXP) {
        SEXP var = VECTOR_ELT(row, j);
        SEXPTYPE type = TYPEOF(var);
        if (type > pinfo->type) {
          switch(type) {
          case LGLSXP:
          case INTSXP:
          case REALSXP:
          case STRSXP:
            pinfo->type = type;
            break;
          default:
            pinfo->type = VECSXP;
          pinfo->scalar = false;
          break;
          }
        }

        // Check that we're still scalar
        if (pinfo->scalar) {
          pinfo->scalar = pinfo->scalar && Rf_length(var) <= 1;
        }
      }
    }
  }

  sort_column_info_by_name(column_info);
  return true;
}

SEXP do_simplify(const ColumnInfoArray* column_info, SEXP x) {
  R_xlen_t ncol = column_info->used;
  R_xlen_t nrow = Rf_length(x);
  SEXP out = PROTECT(allocVector(VECSXP, ncol));

  for (R_xlen_t i = 0; i < nrow; i ++) {
    SEXP col = PROTECT(allocVector(column_info->info[i].type, nrow));
    SET_VECTOR_ELT(out, i, col);
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return out;
}


SEXP C_simplify(SEXP x) {
  SEXP out;
  ColumnInfoArray column_info;
  init_column_info_array(&column_info);

  if (!generate_column_info(&column_info, x)) {
    out = x;
    goto done;
  }

  out = PROTECT(do_simplify(&column_info, x));


  UNPROTECT(1);
done:
  free_column_info_array(&column_info);
  return out;
}

