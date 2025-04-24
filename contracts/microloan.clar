
;; title: microloan


(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-loan-active (err u104))
(define-constant err-loan-not-active (err u105))
(define-constant err-insufficient-collateral (err u106))
(define-constant err-loan-not-due (err u107))
(define-constant err-loan-past-due (err u108))
(define-constant err-invalid-amount (err u109))
(define-constant err-invalid-duration (err u110))
(define-constant err-invalid-rate (err u111))
(define-constant err-endorsement-exists (err u112))
(define-constant err-self-endorsement (err u113))

(define-data-var platform-fee uint u5)
(define-data-var min-collateral-ratio uint u150)

(define-map loans
  { loan-id: uint }
  {
    borrower: principal,
    lender: (optional principal),
    amount: uint,
    collateral: uint,
    duration: uint,
    interest-rate: uint,
    status: (string-ascii 20),
    created-at: uint,
    funded-at: (optional uint),
    due-at: (optional uint)
  }
)

(define-map loan-counter principal uint)

(define-map endorsements
  { endorser: principal, endorsee: principal }
  { trust-score: uint, timestamp: uint }
)

(define-map user-reputation
  { user: principal }
  { score: uint, loans-completed: uint, loans-defaulted: uint }
)

(define-read-only (get-loan (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan (ok loan)
    err-not-found
  )
)

(define-read-only (get-user-loans (user principal))
  (default-to u0 (map-get? loan-counter user))
)

(define-read-only (get-endorsement (endorser principal) (endorsee principal))
  (map-get? endorsements { endorser: endorser, endorsee: endorsee })
)

(define-read-only (get-reputation (user principal))
  (default-to 
    { score: u0, loans-completed: u0, loans-defaulted: u0 }
    (map-get? user-reputation { user: user })
  )
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (get-min-collateral-ratio)
  (var-get min-collateral-ratio)
)

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u100) err-invalid-rate)
    (ok (var-set platform-fee new-fee))
  )
)

(define-public (set-min-collateral-ratio (new-ratio uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (>= new-ratio u100) err-invalid-rate)
    (ok (var-set min-collateral-ratio new-ratio))
  )
)

(define-public (create-loan (amount uint) (collateral uint) (duration uint) (interest-rate uint))
  (let
    (
      (loan-id (+ (get-user-loans tx-sender) u1))
      (collateral-ratio (/ (* collateral u100) amount))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> duration u0) err-invalid-duration)
    (asserts! (<= interest-rate u100) err-invalid-rate)
    (asserts! (>= collateral-ratio (var-get min-collateral-ratio)) err-insufficient-collateral)
    
    (map-set loans
      { loan-id: loan-id }
      {
        borrower: tx-sender,
        lender: none,
        amount: amount,
        collateral: collateral,
        duration: duration,
        interest-rate: interest-rate,
        status: "PENDING",
        created-at: stacks-block-height,
        funded-at: none,
        due-at: none
      }
    )
    
    (map-set loan-counter tx-sender loan-id)
    (ok loan-id)
  )
)

(define-public (fund-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (borrower (get borrower loan))
      (amount (get amount loan))
      (status (get status loan))
    )
    (asserts! (is-eq status "PENDING") err-loan-not-active)
    (asserts! (not (is-eq tx-sender borrower)) err-unauthorized)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan {
        lender: (some tx-sender),
        status: "ACTIVE",
        funded-at: (some stacks-block-height),
        due-at: (some (+ stacks-block-height (get duration loan)))
      })
    )
    (ok true)
  )
)

(define-public (repay-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (borrower (get borrower loan))
      (lender (unwrap! (get lender loan) err-loan-not-active))
      (status (get status loan))
    )
    (asserts! (is-eq tx-sender borrower) err-unauthorized)
    (asserts! (is-eq status "ACTIVE") err-loan-not-active)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan { status: "REPAID" })
    )
    
    (update-reputation borrower true)
    (ok true)
  )
)

(define-public (claim-collateral (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (borrower (get borrower loan))
      (lender (unwrap! (get lender loan) err-loan-not-active))
      (due-at (unwrap! (get due-at loan) err-loan-not-active))
      (status (get status loan))
    )
    (asserts! (is-eq tx-sender lender) err-unauthorized)
    (asserts! (is-eq status "ACTIVE") err-loan-not-active)
    (asserts! (>= stacks-block-height due-at) err-loan-not-due)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan { status: "DEFAULTED" })
    )
    
    (update-reputation borrower false)
    (ok true)
  )
)

(define-public (cancel-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (borrower (get borrower loan))
      (status (get status loan))
    )
    (asserts! (is-eq tx-sender borrower) err-unauthorized)
    (asserts! (is-eq status "PENDING") err-loan-active)
    
    (map-set loans
      { loan-id: loan-id }
      (merge loan { status: "CANCELLED" })
    )
    (ok true)
  )
)

(define-public (endorse-user (user principal) (trust-score uint))
  (begin
    (asserts! (not (is-eq tx-sender user)) err-self-endorsement)
    (asserts! (<= trust-score u100) err-invalid-rate)
    
    (map-set endorsements
      { endorser: tx-sender, endorsee: user }
      { trust-score: trust-score, timestamp: stacks-block-height }
    )
    (ok true)
  )
)

(define-private (update-reputation (user principal) (success bool))
  (let
    (
      (current-rep (get-reputation user))
      (loans-completed (get loans-completed current-rep))
      (loans-defaulted (get loans-defaulted current-rep))
      (new-completed (if success (+ loans-completed u1) loans-completed))
      (new-defaulted (if success loans-defaulted (+ loans-defaulted u1)))
      (new-score (calculate-reputation-score new-completed new-defaulted))
    )
    (map-set user-reputation
      { user: user }
      {
        score: new-score,
        loans-completed: new-completed,
        loans-defaulted: new-defaulted
      }
    )
    true
  )
)

(define-private (calculate-reputation-score (completed uint) (defaulted uint))
  (let
    (
      (total (+ completed defaulted))
      (base-score (* (/ (* completed u100) (if (is-eq total u0) u1 total)) u1))
    )
    (if (> defaulted u0)
      (/ base-score (+ u1 defaulted))
      base-score
    )
  )
)