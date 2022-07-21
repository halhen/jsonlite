#include <Rinternals.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

SEXP C_simplify(SEXP x);

typedef struct {
  const char* name; // Name of column
  SEXPTYPE type; // Most complex type fund for column
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

bool column_info_index_of(const ColumnInfoArray* column_info, const char* name, R_xlen_t* index) {
  if (column_info->used == 0) {
    return false;
  }
  // Binary search for a name match
  size_t low = 0;
  size_t high = column_info->used - 1;
  size_t mid;

  while(low <= high) {
    mid = (low + high) / 2;
    const char* candidate_name = column_info->info[mid].name;

    int strcmp_result = strcmp(name, candidate_name);
    if(strcmp_result == 0) {
      // Match!
      *index = mid;
      return true;
    } else if (strcmp_result > 0) {
      low = mid + 1;
    } else {
      if (mid == 0) {
        break;
      }
      high = mid - 1;
    }
  }

  return false;
}

ColumnInfo* find_column_info(ColumnInfoArray* a, const char* name) {
  R_xlen_t index;
  if (column_info_index_of(a, name, &index)) {
    return a->info + index;
  }

  // Column not found, add a new one
  ColumnInfo new_col = {
    .name = name,
    .type = LGLSXP
  };

  if (a->used >= a->capacity) {
    a->capacity = a->capacity * 2 + 16;
    a->info = realloc(a->info, a->capacity * sizeof(ColumnInfo));
  }

  a->info[a->used++] = new_col;
  qsort(a->info, a->used, sizeof(ColumnInfo), column_info_compare);
  return find_column_info(a, name);
}


// returns true if we can simplify data
bool generate_column_info(ColumnInfoArray* column_info, SEXP original_data) {
  // Make sure rows have no name;
  if (Rf_length(getAttrib(original_data, R_NamesSymbol)) > 0) {
    return false;
  }
  R_xlen_t nrow = Rf_length(original_data);

  for (R_xlen_t i = 0; i < nrow; i ++) {
    SEXP row = VECTOR_ELT(original_data, i);
    SEXP rownames = getAttrib(row, R_NamesSymbol);
    R_xlen_t n_rowcols = Rf_length(rownames);

    if (Rf_length(row) != Rf_length(rownames)) {
      // Make sure all row columns have names
      return false;
    }

    for (R_xlen_t j = 0; j < n_rowcols; j++) {
      ColumnInfo* pinfo = find_column_info(column_info, CHAR(STRING_ELT(rownames, j)));
      // Assign type. Must be one of lgl, int, real, str, or vec. No point in checking deeper than VECSXP
      if (pinfo->type < VECSXP) {
        SEXP var = VECTOR_ELT(row, j);

        if (Rf_length(var) >= 2) {
          pinfo->type = VECSXP;
        } else {
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
            }
          }
        }
      }
    }
  }

  return true;
}

SEXP do_simplify(const ColumnInfoArray* column_info, SEXP x) {
  R_xlen_t ncol = column_info->used;
  R_xlen_t nrow = Rf_length(x);
  SEXP out = PROTECT(allocVector(VECSXP, ncol));

  for (R_xlen_t i = 0; i < column_info->used; i ++) {
    SEXP col = PROTECT(allocVector(column_info->info[i].type, nrow));
    SET_VECTOR_ELT(out, i, col);
    UNPROTECT(1);

    // TODO: Set default NA
  }

  for (R_xlen_t rownum = 0; rownum < nrow; rownum++) {
    SEXP row = VECTOR_ELT(x, rownum);
    SEXP rownames = getAttrib(row, R_NamesSymbol);
    R_xlen_t n_rowcols = Rf_length(rownames);

    for (R_xlen_t colnum = 0; colnum < n_rowcols; colnum++) {
      const char* colname = CHAR(STRING_ELT(rownames, colnum));

      R_xlen_t index;
      if (column_info_index_of(column_info, colname, &index)) {
        SEXP col = VECTOR_ELT(out, index);
        if (Rf_length(col) > 0) {
          if (TYPEOF(col) == LGLSXP) {
            LOGICAL(col)[rownum] = Rf_asLogical(VECTOR_ELT(row, colnum));
          } else if (TYPEOF(col) == INTSXP) {
            INTEGER(col)[rownum] = Rf_asInteger(VECTOR_ELT(row, colnum));
          } else if (TYPEOF(col) == REALSXP) {
            REAL(col)[rownum] = Rf_asReal(VECTOR_ELT(row, colnum));
          }  else if (TYPEOF(col) == STRSXP) {
            SET_STRING_ELT(col, rownum, Rf_asChar(VECTOR_ELT(row, colnum)));
          } else {
            SET_VECTOR_ELT(col, rownum, C_simplify(VECTOR_ELT(row, colnum)));
          }
        }
      }
    }
  }


  UNPROTECT(1);
  return out;
}


SEXP C_simplify(SEXP x) {
  if (TYPEOF(x) != VECSXP) {
    return x;
  }

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

