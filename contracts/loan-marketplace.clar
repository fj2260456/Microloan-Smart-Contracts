;; Loan Transfer Marketplace Contract
;; Enables secondary trading of active loans between lenders

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u200))
(define-constant err-not-found (err u201))
(define-constant err-invalid-price (err u202))
(define-constant err-listing-exists (err u203))
(define-constant err-self-purchase (err u204))
(define-constant err-loan-not-active (err u205))
(define-constant err-insufficient-funds (err u206))
(define-constant err-transfer-failed (err u207))
(define-constant err-listing-expired (err u208))
(define-constant err-invalid-discount (err u209))

;; Data variables
(define-data-var marketplace-fee-rate uint u25) ;; 2.5% fee
(define-data-var min-listing-duration uint u1440) ;; 1 day in blocks
(define-data-var max-listing-duration uint u14400) ;; 10 days in blocks
(define-data-var marketplace-active bool true)
(define-data-var total-volume uint u0)
(define-data-var next-listing-id uint u1)

;; Data maps
(define-map loan-listings
  { listing-id: uint }
  {
    loan-id: uint,
    seller: principal,
    asking-price: uint,
    remaining-value: uint,
    discount-percent: uint,
    created-at: uint,
    expires-at: uint,
    status: (string-ascii 10),
    loan-contract: principal
  }
)

(define-map seller-listings
  { seller: principal, loan-id: uint }
  { listing-id: uint, active: bool }
)

(define-map marketplace-stats
  { user: principal }
  {
    loans-sold: uint,
    loans-purchased: uint,
    total-sold-value: uint,
    total-purchased-value: uint,
    last-activity: uint
  }
)

(define-map featured-listings
  { featured-slot: uint }
  { listing-id: uint, expires-at: uint }
)

;; Read-only functions
(define-read-only (get-listing (listing-id uint))
  (map-get? loan-listings { listing-id: listing-id })
)

(define-read-only (get-seller-listing (seller principal) (loan-id uint))
  (map-get? seller-listings { seller: seller, loan-id: loan-id })
)

(define-read-only (get-marketplace-stats (user principal))
  (default-to 
    { loans-sold: u0, loans-purchased: u0, total-sold-value: u0, total-purchased-value: u0, last-activity: u0 }
    (map-get? marketplace-stats { user: user })
  )
)

(define-read-only (get-marketplace-info)
  {
    fee-rate: (var-get marketplace-fee-rate),
    min-duration: (var-get min-listing-duration),
    max-duration: (var-get max-listing-duration),
    active: (var-get marketplace-active),
    total-volume: (var-get total-volume),
    next-listing-id: (var-get next-listing-id)
  }
)

(define-read-only (calculate-transfer-cost (listing-id uint))
  (match (get-listing listing-id)
    listing 
    (let
      (
        (asking-price (get asking-price listing))
        (fee (/ (* asking-price (var-get marketplace-fee-rate)) u1000))
      )
      (ok { price: asking-price, fee: fee, total: (+ asking-price fee) })
    )
    err-not-found
  )
)

;; Public functions
(define-public (create-listing (loan-id uint) (asking-price uint) (duration-blocks uint) (loan-contract principal))
  (let
    (
      (listing-id (var-get next-listing-id))
      (current-block stacks-block-height)
      (expires-at (+ current-block duration-blocks))
    )
    ;; Validate inputs
    (asserts! (var-get marketplace-active) err-unauthorized)
    (asserts! (> asking-price u0) err-invalid-price)
    (asserts! (>= duration-blocks (var-get min-listing-duration)) err-invalid-discount)
    (asserts! (<= duration-blocks (var-get max-listing-duration)) err-invalid-discount)
    (asserts! (is-none (map-get? seller-listings { seller: tx-sender, loan-id: loan-id })) err-listing-exists)
    
    ;; Calculate remaining loan value and discount
    (let
      (
        (remaining-value asking-price) ;; Simplified - in real implementation would calculate from loan terms
        (discount-percent (if (< asking-price remaining-value) 
                            (/ (* (- remaining-value asking-price) u100) remaining-value) 
                            u0))
      )
      
      ;; Create listing
      (map-set loan-listings
        { listing-id: listing-id }
        {
          loan-id: loan-id,
          seller: tx-sender,
          asking-price: asking-price,
          remaining-value: remaining-value,
          discount-percent: discount-percent,
          created-at: current-block,
          expires-at: expires-at,
          status: "ACTIVE",
          loan-contract: loan-contract
        }
      )
      
      ;; Link seller to listing
      (map-set seller-listings
        { seller: tx-sender, loan-id: loan-id }
        { listing-id: listing-id, active: true }
      )
      
      ;; Increment listing counter
      (var-set next-listing-id (+ listing-id u1))
      
      (ok listing-id)
    )
  )
)

(define-public (purchase-loan (listing-id uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (seller (get seller listing))
      (asking-price (get asking-price listing))
      (loan-id (get loan-id listing))
      (status (get status listing))
      (expires-at (get expires-at listing))
      (current-block stacks-block-height)
    )
    ;; Validate purchase
    (asserts! (var-get marketplace-active) err-unauthorized)
    (asserts! (is-eq status "ACTIVE") err-loan-not-active)
    (asserts! (< current-block expires-at) err-listing-expired)
    (asserts! (not (is-eq tx-sender seller)) err-self-purchase)
    
    ;; Calculate fees
    (let
      (
        (marketplace-fee (/ (* asking-price (var-get marketplace-fee-rate)) u1000))
        (seller-amount (- asking-price marketplace-fee))
      )
      
      ;; Update listing status
      (map-set loan-listings
        { listing-id: listing-id }
        (merge listing { 
          status: "SOLD",
          expires-at: current-block 
        })
      )
      
      ;; Deactivate seller listing
      (map-set seller-listings
        { seller: seller, loan-id: loan-id }
        { listing-id: listing-id, active: false }
      )
      
      ;; Update marketplace stats
      (update-user-stats seller true asking-price)
      (update-user-stats tx-sender false asking-price)
      
      ;; Update total volume
      (var-set total-volume (+ (var-get total-volume) asking-price))
      
      ;; Execute the actual loan transfer would happen here
      ;; In real implementation, this would call the main contract to transfer loan ownership
      
      (ok { 
        loan-id: loan-id, 
        price: asking-price, 
        fee: marketplace-fee,
        new-owner: tx-sender
      })
    )
  )
)

(define-public (cancel-listing (listing-id uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (seller (get seller listing))
      (loan-id (get loan-id listing))
      (status (get status listing))
    )
    (asserts! (is-eq tx-sender seller) err-unauthorized)
    (asserts! (is-eq status "ACTIVE") err-loan-not-active)
    
    ;; Update listing
    (map-set loan-listings
      { listing-id: listing-id }
      (merge listing { status: "CANCELLED" })
    )
    
    ;; Deactivate seller listing
    (map-set seller-listings
      { seller: seller, loan-id: loan-id }
      { listing-id: listing-id, active: false }
    )
    
    (ok true)
  )
)

(define-public (update-listing-price (listing-id uint) (new-price uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (seller (get seller listing))
      (status (get status listing))
      (remaining-value (get remaining-value listing))
    )
    (asserts! (is-eq tx-sender seller) err-unauthorized)
    (asserts! (is-eq status "ACTIVE") err-loan-not-active)
    (asserts! (> new-price u0) err-invalid-price)
    
    (let
      (
        (new-discount (if (< new-price remaining-value)
                        (/ (* (- remaining-value new-price) u100) remaining-value)
                        u0))
      )
      (map-set loan-listings
        { listing-id: listing-id }
        (merge listing { 
          asking-price: new-price,
          discount-percent: new-discount
        })
      )
      (ok new-price)
    )
  )
)

(define-public (extend-listing (listing-id uint) (additional-blocks uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (seller (get seller listing))
      (status (get status listing))
      (current-expires (get expires-at listing))
      (max-duration (var-get max-listing-duration))
    )
    (asserts! (is-eq tx-sender seller) err-unauthorized)
    (asserts! (is-eq status "ACTIVE") err-loan-not-active)
    (asserts! (> additional-blocks u0) err-invalid-discount)
    
    (let
      (
        (new-expires (+ current-expires additional-blocks))
        (max-allowed (+ (get created-at listing) max-duration))
      )
      (asserts! (<= new-expires max-allowed) err-invalid-discount)
      
      (map-set loan-listings
        { listing-id: listing-id }
        (merge listing { expires-at: new-expires })
      )
      (ok new-expires)
    )
  )
)

(define-public (feature-listing (listing-id uint) (slot uint) (duration-blocks uint))
  (let
    (
      (listing (unwrap! (get-listing listing-id) err-not-found))
      (current-block stacks-block-height)
      (expires-at (+ current-block duration-blocks))
    )
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (is-eq (get status listing) "ACTIVE") err-loan-not-active)
    (asserts! (<= slot u9) err-invalid-discount) ;; Max 10 featured slots
    
    (map-set featured-listings
      { featured-slot: slot }
      { listing-id: listing-id, expires-at: expires-at }
    )
    (ok true)
  )
)

;; Administrative functions
(define-public (set-marketplace-parameters (fee-rate uint) (min-duration uint) (max-duration uint) (active bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (<= fee-rate u100) err-invalid-discount) ;; Max 10% fee
    (asserts! (< min-duration max-duration) err-invalid-discount)
    
    (var-set marketplace-fee-rate fee-rate)
    (var-set min-listing-duration min-duration)
    (var-set max-listing-duration max-duration)
    (var-set marketplace-active active)
    (ok true)
  )
)

(define-public (get-active-listings-by-seller (seller principal))
  (ok (var-get next-listing-id)) ;; Simplified - would return actual list in full implementation
)

(define-public (get-listings-by-criteria (min-price uint) (max-price uint) (max-discount uint))
  (ok (var-get next-listing-id)) ;; Simplified - would return filtered results
)

;; Private functions
(define-private (update-user-stats (user principal) (is-seller bool) (amount uint))
  (let
    (
      (current-stats (get-marketplace-stats user))
      (current-block stacks-block-height)
    )
    (if is-seller
      (map-set marketplace-stats
        { user: user }
        {
          loans-sold: (+ (get loans-sold current-stats) u1),
          loans-purchased: (get loans-purchased current-stats),
          total-sold-value: (+ (get total-sold-value current-stats) amount),
          total-purchased-value: (get total-purchased-value current-stats),
          last-activity: current-block
        }
      )
      (map-set marketplace-stats
        { user: user }
        {
          loans-sold: (get loans-sold current-stats),
          loans-purchased: (+ (get loans-purchased current-stats) u1),
          total-sold-value: (get total-sold-value current-stats),
          total-purchased-value: (+ (get total-purchased-value current-stats) amount),
          last-activity: current-block
        }
      )
    )
    true
  )
)

(define-read-only (get-featured-listing (slot uint))
  (map-get? featured-listings { featured-slot: slot })
)

(define-read-only (is-listing-featured (listing-id uint))
  (let
    (
      (current-block stacks-block-height)
    )
    ;; Check all featured slots - simplified implementation
    (or 
      (match (get-featured-listing u0)
        featured (and (is-eq (get listing-id featured) listing-id) (> (get expires-at featured) current-block))
        false
      )
      (match (get-featured-listing u1)
        featured (and (is-eq (get listing-id featured) listing-id) (> (get expires-at featured) current-block))
        false
      )
    )
  )
)
