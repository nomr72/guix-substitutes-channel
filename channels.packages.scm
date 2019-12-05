; must be set to correct architecture
(define required-system "i686-linux")
; lists packages that must have built substitutes
(define required-packages (list "ungoogled-chromium"))

(define cuirass-server "https://ci.guix.gnu.org")

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
             (string-append cuirass-server "/api/evaluations?nr=1000"))))))

(define (evaluation-complete? number system packages)
  "Return true if evaluation NUMBER completed on SYSTEM for all PACKAGES."
  (let ((builds (json->scm
		  (http-fetch
		    (string-append
		      cuirass-server
		      "/api/latestbuilds?nr=1000&evaluation="
		      (number->string number)
		      "&system="
		      system
		      "&status=succeeded")))))
    ; for all PACKAGES, assert that there is an entry in response
    ; such that "job" starts with the package, and "buildstatus" = 0
    (every
      (lambda (package)
	(vector-any
	  (lambda (build)
	    (let ((job (assoc-ref build "job"))
		  (buildstatus (assoc-ref build "buildstatus")))
	      (and
		(= buildstatus 0)
		(>= (string-length job) (string-length package))
		(string=
		  package
		  (string-take job (string-length package))))))
	  builds))
      packages)))

(define (latest-commit-successfully-built system packages)
  "Return the latest commit for which substitutes on SYSTEM are available for PACKAGES."
  (let* ((evaluations (latest-evaluations "guix-master"))
         (candidates  (filter-map (lambda (json)
                                    (match (vector->list (assoc-ref json "checkouts"))
                                      ((checkout)
                                       (cons (assoc-ref json "id")
                                             (assoc-ref checkout "commit")))
                                      (_ #f)))
                                  evaluations)))
    (any (match-lambda
            ((evaluation . commit)
	     (display (string-append "Checking " (string-take commit 7) " ...\n"))
             (and (evaluation-complete? evaluation system packages)
                  commit)))
          candidates)))
(display (string-append "Requiring " required-system " builds for " (string-join required-packages", ") ".\n"))
(display (string-append "Querying " cuirass-server " for build statuses...\n"))

; Pull the latest commit fully built on the cuirass server.
; WARNING: This could downgrade your system!
(list (channel
       (name 'guix)
       (url "https://git.savannah.gnu.org/git/guix.git")
       (commit (pk 'commit (latest-commit-successfully-built
			     required-system
			     required-packages)))))
