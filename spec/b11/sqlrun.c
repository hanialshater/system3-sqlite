/* Benchmark 11 reference driver (B10's, plus a "~~~" line after each input line's output): runs an SQL script against an in-memory SQLite database and prints what happens.
   One statement per line (a line may hold several; a trigger body stays on its line). For each statement:
   a "# col|col" line with its column names if it has columns, one line per row ("|" between values; NULL, integers,
   SQLite's own text for reals, text as is, blobs as X'hex'), "changes: N" after a statement that writes, and
   "error: <message>" when it fails, after which the rest of that line is skipped. Usage: sqlrun script.sql */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sqlite3.h"

static void put_value(sqlite3_stmt *st, int i) {
  switch (sqlite3_column_type(st, i)) {
  case SQLITE_NULL: fputs("NULL", stdout); break;
  case SQLITE_INTEGER: printf("%lld", (long long)sqlite3_column_int64(st, i)); break;
  case SQLITE_BLOB: {
    const unsigned char *b = (const unsigned char *)sqlite3_column_blob(st, i);
    int n = sqlite3_column_bytes(st, i), k;
    fputs("X'", stdout);
    for (k = 0; k < n; k++) printf("%02X", b[k]);
    fputs("'", stdout);
    break;
  }
  default: fputs((const char *)sqlite3_column_text(st, i), stdout); break;
  }
}

static void run_line(sqlite3 *db, const char *sql) {
  const char *tail = sql;
  while (tail && *tail) {
    sqlite3_stmt *st = 0;
    const char *next = 0;
    int rc = sqlite3_prepare_v2(db, tail, -1, &st, &next);
    if (rc != SQLITE_OK) { printf("error: %s\n", sqlite3_errmsg(db)); return; }
    if (!st) { tail = next; continue; } /* whitespace or a comment */
    int ncol = sqlite3_column_count(st), i;
    if (ncol > 0) {
      fputs("# ", stdout);
      for (i = 0; i < ncol; i++) { if (i) putchar('|'); fputs(sqlite3_column_name(st, i), stdout); }
      putchar('\n');
    }
    while ((rc = sqlite3_step(st)) == SQLITE_ROW) {
      for (i = 0; i < ncol; i++) { if (i) putchar('|'); put_value(st, i); }
      putchar('\n');
    }
    if (rc != SQLITE_DONE) { printf("error: %s\n", sqlite3_errmsg(db)); sqlite3_finalize(st); return; }
    if (ncol == 0 && !sqlite3_stmt_readonly(st)) printf("changes: %d\n", sqlite3_changes(db));
    sqlite3_finalize(st);
    tail = next;
  }
}

int main(int argc, char **argv) {
  if (argc != 2) { fprintf(stderr, "usage: sqlrun script.sql\n"); return 2; }
  FILE *f = fopen(argv[1], "rb");
  if (!f) { fprintf(stderr, "sqlrun: cannot open %s\n", argv[1]); return 2; }
  sqlite3 *db = 0;
  if (sqlite3_initialize() != SQLITE_OK || sqlite3_open(":memory:", &db) != SQLITE_OK) { fprintf(stderr, "sqlrun: cannot open a database\n"); return 2; }
  static char line[1 << 16];
  while (fgets(line, sizeof line, f)) {
    size_t n = strlen(line);
    while (n && (line[n - 1] == '\n' || line[n - 1] == '\r')) line[--n] = 0;
    if (n) { run_line(db, line); puts("~~~"); }
  }
  fclose(f);
  sqlite3_close(db);
  return 0;
}
