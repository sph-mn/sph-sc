; example usage: sc-documentor a.sc b.sc --config="config.scm"
(add
  ; additional expressions to include. for example for preprocessor generated identifiers
  (declare (db-ids-length a) (size-t db-ids-t*)))

(remove
  ; identifier names to remove, regexp patterns
  "db-mdb-.*" "mi-list-.*" "db-status-notfound-if-notfound" "imht-set-.*")

(replace
  ; identifier replacements. regexp pattern and replacement string alternatingly
  "^ui8" "uint8_t"
  "^ui16" "uint16_t"
  "^ui32" "uint32_t"
  "^ui64" "uint64_t"
  "^i8" "int8_t"
  "^i16" "int16_t" "^i32" "int32_t" "^i64" "int64_t" "^f32" "double" "boolean" "uint8_t")
