
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
(define-constant err-no-matches (err u117))
(define-constant err-match-exists (err u118))
(define-constant err-invalid-criteria (err u119))
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





(define-public (get-loan-status (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (status (get status loan))
    )
    (ok status)
  )
)
(define-public (get-loan-collateral (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (collateral (get collateral loan))
    )
    (ok collateral)
  )
)
(define-public (get-loan-amount (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (amount (get amount loan))
    )
    (ok amount)
  )
)
(define-public (get-loan-borrower (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (borrower (get borrower loan))
    )
    (ok borrower)
  )
)
(define-public (get-loan-lender (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (lender (unwrap! (get lender loan) err-loan-not-active))
    )
    (ok lender)
  )
)


(define-constant err-extension-exists (err u114))
(define-constant err-invalid-extension (err u115))

(define-map loan-extensions 
  { loan-id: uint }
  { 
    requested-blocks: uint,
    status: (string-ascii 10)
  }
)

(define-public (request-extension (loan-id uint) (additional-blocks uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (borrower (get borrower loan))
      (status (get status loan))
    )
    (asserts! (is-eq tx-sender borrower) err-unauthorized)
    (asserts! (is-eq status "ACTIVE") err-loan-not-active)
    (asserts! (> additional-blocks u0) err-invalid-extension)
    (asserts! (is-none (map-get? loan-extensions { loan-id: loan-id })) err-extension-exists)

    (map-set loan-extensions
      { loan-id: loan-id }
      {
        requested-blocks: additional-blocks,
        status: "PENDING"
      }
    )
    (ok true)
  )
)

(define-public (approve-extension (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (extension (unwrap! (map-get? loan-extensions { loan-id: loan-id }) err-not-found))
      (lender (unwrap! (get lender loan) err-loan-not-active))
      (current-due (unwrap! (get due-at loan) err-loan-not-active))
    )
    (asserts! (is-eq tx-sender lender) err-unauthorized)
    (asserts! (is-eq (get status extension) "PENDING") err-not-found)

    (map-set loans
      { loan-id: loan-id }
      (merge loan {
        due-at: (some (+ current-due (get requested-blocks extension)))
      })
    )

    (map-set loan-extensions
      { loan-id: loan-id }
      (merge extension { status: "APPROVED" })
    )
    (ok true)
  )
)


(define-constant err-invalid-refinance (err u116))

(define-map refinance-requests
  { loan-id: uint }
  {
    new-amount: uint,
    new-duration: uint,
    new-interest-rate: uint,
    status: (string-ascii 10)
  }
)

(define-public (request-refinance (loan-id uint) (new-amount uint) (new-duration uint) (new-interest-rate uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (borrower (get borrower loan))
      (status (get status loan))
    )
    (asserts! (is-eq tx-sender borrower) err-unauthorized)
    (asserts! (is-eq status "ACTIVE") err-loan-not-active)
    (asserts! (> new-amount u0) err-invalid-amount)
    (asserts! (> new-duration u0) err-invalid-duration)
    (asserts! (<= new-interest-rate u100) err-invalid-rate)

    (map-set refinance-requests
      { loan-id: loan-id }
      {
        new-amount: new-amount,
        new-duration: new-duration,
        new-interest-rate: new-interest-rate,
        status: "PENDING"
      }
    )
    (ok true)
  )
)

(define-public (approve-refinance (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (refinance (unwrap! (map-get? refinance-requests { loan-id: loan-id }) err-not-found))
      (lender (unwrap! (get lender loan) err-loan-not-active))
    )
    (asserts! (is-eq tx-sender lender) err-unauthorized)
    (asserts! (is-eq (get status refinance) "PENDING") err-not-found)

    (map-set loans
      { loan-id: loan-id }
      (merge loan {
        amount: (get new-amount refinance),
        duration: (get new-duration refinance),
        interest-rate: (get new-interest-rate refinance),
        due-at: (some (+ stacks-block-height (get new-duration refinance)))
      })
    )

    (map-set refinance-requests
      { loan-id: loan-id }
      (merge refinance { status: "APPROVED" })
    )
    (ok true)
  )
)



(define-map lender-preferences
  { lender: principal }
  {
    max-amount: uint,
    min-interest-rate: uint,
    max-duration: uint,
    min-reputation-score: uint,
    auto-fund: bool
  }
)

(define-map loan-matches
  { loan-id: uint }
  {
    matched-lenders: (list 10 principal),
    match-scores: (list 10 uint),
    best-match: (optional principal)
  }
)

(define-data-var matching-enabled bool true)

(define-public (set-lender-preferences (max-amount uint) (min-interest-rate uint) (max-duration uint) (min-reputation-score uint) (auto-fund bool))
  (begin
    (asserts! (> max-amount u0) err-invalid-amount)
    (asserts! (<= min-interest-rate u100) err-invalid-rate)
    (asserts! (> max-duration u0) err-invalid-duration)
    (asserts! (<= min-reputation-score u100) err-invalid-rate)
    
    (map-set lender-preferences
      { lender: tx-sender }
      {
        max-amount: max-amount,
        min-interest-rate: min-interest-rate,
        max-duration: max-duration,
        min-reputation-score: min-reputation-score,
        auto-fund: auto-fund
      }
    )
    (ok true)
  )
)

(define-public (find-loan-matches (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (borrower (get borrower loan))
      (amount (get amount loan))
      (interest-rate (get interest-rate loan))
      (duration (get duration loan))
      (borrower-rep (get score (get-reputation borrower)))
      (status (get status loan))
    )
    (asserts! (is-eq status "PENDING") err-loan-not-active)
    (asserts! (var-get matching-enabled) err-unauthorized)
    
    (let
      (
        (match-result (find-compatible-lenders amount interest-rate duration borrower-rep))
      )
      (map-set loan-matches
        { loan-id: loan-id }
        {
          matched-lenders: (get lenders match-result),
          match-scores: (get scores match-result),
          best-match: (get best-lender match-result)
        }
      )
      (ok match-result)
    )
  )
)

(define-public (auto-fund-matched-loan (loan-id uint))
  (let
    (
      (loan (unwrap! (get-loan loan-id) err-not-found))
      (matches (unwrap! (map-get? loan-matches { loan-id: loan-id }) err-no-matches))
      (best-lender (unwrap! (get best-match matches) err-no-matches))
      (lender-prefs (unwrap! (map-get? lender-preferences { lender: best-lender }) err-not-found))
    )
    (asserts! (is-eq tx-sender best-lender) err-unauthorized)
    (asserts! (get auto-fund lender-prefs) err-unauthorized)
    (asserts! (is-eq (get status loan) "PENDING") err-loan-not-active)
    
    (fund-loan loan-id)
  )
)

(define-public (get-loan-matches (loan-id uint))
  (match (map-get? loan-matches { loan-id: loan-id })
    matches (ok matches)
    err-no-matches
  )
)

(define-public (get-lender-preferences (lender principal))
  (match (map-get? lender-preferences { lender: lender })
    prefs (ok prefs)
    err-not-found
  )
)

(define-public (toggle-matching (enabled bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok (var-set matching-enabled enabled))
  )
)

(define-private (find-compatible-lenders (amount uint) (interest-rate uint) (duration uint) (borrower-reputation uint))
  (let
    (
      (sample-lenders (list tx-sender))
      (empty-scores (list u0))
    )
    {
      lenders: sample-lenders,
      scores: empty-scores,
      best-lender: (some tx-sender)
    }
  )
)

(define-private (calculate-match-score (lender-prefs (tuple (max-amount uint) (min-interest-rate uint) (max-duration uint) (min-reputation-score uint) (auto-fund bool))) (loan-amount uint) (loan-interest uint) (loan-duration uint) (borrower-rep uint))
  (let
    (
      (amount-score (if (<= loan-amount (get max-amount lender-prefs)) u25 u0))
      (interest-score (if (>= loan-interest (get min-interest-rate lender-prefs)) u25 u0))
      (duration-score (if (<= loan-duration (get max-duration lender-prefs)) u25 u0))
      (reputation-score (if (>= borrower-rep (get min-reputation-score lender-prefs)) u25 u0))
    )
    (+ amount-score interest-score duration-score reputation-score)
  )
)

(define-read-only (is-matching-enabled)
  (var-get matching-enabled)
)

(define-public (remove-lender-preferences)
  (begin
    (map-delete lender-preferences { lender: tx-sender })
    (ok true)
  )
)

(define-public (get-compatible-loans-for-lender)
  (let
    (
      (lender-prefs (unwrap! (map-get? lender-preferences { lender: tx-sender }) err-not-found))
    )
    (ok lender-prefs)
  )
)