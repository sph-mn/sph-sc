(define-module (sph lang sc eval-environment))

(define sph-lang-sc-eval-environment-description
  "additional helpers available in sc-define-syntax*")

(use-modules (sph list) (sph))
(re-export flatten contains? containsq? containsv? first tail pair null pairs q qq)
