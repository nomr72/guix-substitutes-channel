(use-modules (guix http-client)
             (json)
             (srfi srfi-1)
	     (srfi srfi-43)
             (ice-9 match))

(define (latest-evaluations jobset)
  "Return the latest evaluations of JOBSET."
  (filter (lambda (json)
            (string=? (assoc-ref json "specification") jobset))
	  (vector->list
	   (json->scm
            (http-fetch
             "https://berlin.guixsd.org/api/evaluations?nr=100")))))

(define (evaluation-complete? number)
  "Return true if evaluation NUMBER completed and all its builds were
successful."
  (let ((builds (json->scm
                 (http-fetch
                  (string-append
                   "https://berlin.guixsd.org/api/latestbuilds?nr=30&evaluation="
                   (number->string number))))))
    (and (vector-every (lambda (build)
             ;; Zero means build success.
             (= (assoc-ref build "buildstatus") 0))
           builds)
	 (> (vector-length builds) 3))
    ))

(define (latest-commit-successfully-built)
  "Return the latest commit for which substitutes are (potentially)
available."
  (let* ((evaluations (latest-evaluations "guix-modular-master"))
         (candidates  (filter-map (lambda (json)
                                    (match (vector->list (assoc-ref json "checkouts"))
                                      ((checkout)
                                       (cons (assoc-ref json "id")
                                             (assoc-ref checkout "commit")))
                                      (_ #f)))
                                  evaluations)))
    (any (match-lambda
            ((evaluation . commit)
             (and (evaluation-complete? evaluation)
                  commit)))
          candidates)))

; Pull the latest commit fully built on berlin.guixsd.org.
; WARNING: This could downgrade your system!
(list (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (commit (pk 'commit (latest-commit-successfully-built)))))
