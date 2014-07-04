(includep "stdio.h")
(include "../lib/mdb.h")

(define (main argc argv) (b32 b32 b16*)
  (define
    rc b32
    env MDB_env*
    txn MDB_txn*
    cursor MDB_cursor*
    dbi MDB_dbi
    key MDB_val
    data MDB_val)

  (set! rc (mdb-env-create (pointer-ref env)))
  (struct-ref test key)


  )
