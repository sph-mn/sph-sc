(define-module (sph lang sc eval-environment))

(define sph-lang-sc-eval-environment-description
  "additional helpers available in sc-define-syntax*")

(use-modules (sph list) (sph hashtable) (sph) (sph lang sc))

(re-export flatten contains?
  containsq? containsv? first map-with-index tail pair null pairs q qq debug-log sc-gensym sc-syntax?)
