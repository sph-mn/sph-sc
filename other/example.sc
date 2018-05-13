(pre-include "stdio.h")
(pre-include "../lib/mdb.h")

(define (main argc argv) (int int int*)
  (declare
    rc int
    env MDB_env*
    txn MDB_txn*
    cursor MDB_cursor*
    dbi MDB_dbi
    key MDB_val
    data MDB_val)
  (set rc (mdb-env-create (pointer-get env)))
  (struct-get test key))
