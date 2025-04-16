;; Technician Verification Contract
;; Validates qualified service providers

(define-data-var contract-owner principal tx-sender)

;; Data structure for technician information
(define-map technicians
  { address: principal }
  {
    name: (string-ascii 64),
    company: (string-ascii 64),
    certification-ids: (list 10 (string-ascii 32)),
    certification-expiry: uint,
    specializations: (list 5 (string-ascii 32)),
    is-active: bool,
    rating: uint,
    service-count: uint
  }
)

;; Approved certifying organizations
(define-map certifying-orgs
  { org-id: (string-ascii 32) }
  { is-approved: bool }
)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED u300)
(define-constant ERR-TECHNICIAN-EXISTS u301)
(define-constant ERR-TECHNICIAN-NOT-FOUND u302)
(define-constant ERR-CERTIFICATION-EXPIRED u303)
(define-constant ERR-ORG-NOT-APPROVED u304)
(define-constant ERR-INVALID-RATING u305)

;; Initialize approved certifying organizations
(define-private (initialize-orgs)
  (begin
    (map-set certifying-orgs { org-id: "NATE" } { is-approved: true })
    (map-set certifying-orgs { org-id: "HVAC-Excellence" } { is-approved: true })
    (map-set certifying-orgs { org-id: "EPA" } { is-approved: true })
    true
  )
)

;; Register a new technician
(define-public (register-technician
                (name (string-ascii 64))
                (company (string-ascii 64))
                (certification-ids (list 10 (string-ascii 32)))
                (certification-expiry uint)
                (specializations (list 5 (string-ascii 32))))
  (let ((technician-principal tx-sender))
    ;; Check if technician already exists
    (asserts! (is-none (map-get? technicians { address: technician-principal })) (err ERR-TECHNICIAN-EXISTS))

    ;; Check if certification is not expired
    (asserts! (> certification-expiry block-height) (err ERR-CERTIFICATION-EXPIRED))

    ;; Register the technician
    (map-set technicians
      { address: technician-principal }
      {
        name: name,
        company: company,
        certification-ids: certification-ids,
        certification-expiry: certification-expiry,
        specializations: specializations,
        is-active: true,
        rating: u0,
        service-count: u0
      }
    )

    (ok true)
  )
)

;; Update technician information
(define-public (update-technician
                (name (string-ascii 64))
                (company (string-ascii 64))
                (specializations (list 5 (string-ascii 32))))
  (let ((technician-principal tx-sender)
        (technician-info (map-get? technicians { address: technician-principal })))

    ;; Check if technician exists
    (asserts! (is-some technician-info) (err ERR-TECHNICIAN-NOT-FOUND))

    ;; Update technician information
    (map-set technicians
      { address: technician-principal }
      (merge (unwrap-panic technician-info)
        {
          name: name,
          company: company,
          specializations: specializations
        }
      )
    )

    (ok true)
  )
)

;; Update technician certification
(define-public (update-certification
                (certification-ids (list 10 (string-ascii 32)))
                (certification-expiry uint))
  (let ((technician-principal tx-sender)
        (technician-info (map-get? technicians { address: technician-principal })))

    ;; Check if technician exists
    (asserts! (is-some technician-info) (err ERR-TECHNICIAN-NOT-FOUND))

    ;; Check if certification is not expired
    (asserts! (> certification-expiry block-height) (err ERR-CERTIFICATION-EXPIRED))

    ;; Update certification information
    (map-set technicians
      { address: technician-principal }
      (merge (unwrap-panic technician-info)
        {
          certification-ids: certification-ids,
          certification-expiry: certification-expiry
        }
      )
    )

    (ok true)
  )
)

;; Verify if a technician is qualified
(define-read-only (is-qualified (technician principal))
  (let ((technician-info (map-get? technicians { address: technician })))
    (and
      (is-some technician-info)
      (get is-active (unwrap-panic technician-info))
      (> (get certification-expiry (unwrap-panic technician-info)) block-height)
    )
  )
)

;; Get technician details
(define-read-only (get-technician (technician principal))
  (map-get? technicians { address: technician })
)

;; Rate a technician (called by system owners after service)
(define-public (rate-technician (technician principal) (rating uint))
  (let ((technician-info (map-get? technicians { address: technician })))
    ;; Check if technician exists
    (asserts! (is-some technician-info) (err ERR-TECHNICIAN-NOT-FOUND))

    ;; Check if rating is valid (1-5)
    (asserts! (and (>= rating u1) (<= rating u5)) (err ERR-INVALID-RATING))

    ;; Calculate new average rating
    (let ((current-rating (get rating (unwrap-panic technician-info)))
          (service-count (get service-count (unwrap-panic technician-info)))
          (new-service-count (+ service-count u1))
          (new-rating (/ (+ (* current-rating service-count) rating) new-service-count)))

      ;; Update technician rating
      (map-set technicians
        { address: technician }
        (merge (unwrap-panic technician-info)
          {
            rating: new-rating,
            service-count: new-service-count
          }
        )
      )

      (ok true)
    )
  )
)

;; Deactivate a technician (admin only)
(define-public (deactivate-technician (technician principal))
  (let ((technician-info (map-get? technicians { address: technician })))
    ;; Check if caller is contract owner
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))

    ;; Check if technician exists
    (asserts! (is-some technician-info) (err ERR-TECHNICIAN-NOT-FOUND))

    ;; Deactivate technician
    (map-set technicians
      { address: technician }
      (merge (unwrap-panic technician-info) { is-active: false })
    )

    (ok true)
  )
)

;; Add a new approved certifying organization (admin only)
(define-public (add-certifying-org (org-id (string-ascii 32)))
  (begin
    ;; Check if caller is contract owner
    (asserts! (is-eq tx-sender (var-get contract-owner)) (err ERR-NOT-AUTHORIZED))

    ;; Add organization
    (map-set certifying-orgs { org-id: org-id } { is-approved: true })

    (ok true)
  )
)

;; Check if a certifying organization is approved
(define-read-only (is-org-approved (org-id (string-ascii 32)))
  (default-to false (get is-approved (map-get? certifying-orgs { org-id: org-id })))
)
