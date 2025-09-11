;; Loan Performance Tracker Contract
;; Provides real-time analytics and performance metrics for the microloan platform
;; Tracks KPIs, success rates, and market trends

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u300))
(define-constant err-not-found (err u301))
(define-constant err-invalid-period (err u302))
(define-constant err-invalid-threshold (err u303))

;; Data Variables
(define-data-var analytics-enabled bool true)
(define-data-var performance-window uint u1440) ;; 24 hours in blocks
(define-data-var min-sample-size uint u10)

;; Global Performance Metrics
(define-map platform-metrics
  uint ;; block-period
  {
    total-loans: uint,
    active-loans: uint,
    completed-loans: uint,
    defaulted-loans: uint,
    total-volume: uint,
    total-interest-earned: uint,
    success-rate: uint, ;; percentage * 100
    average-interest-rate: uint,
    average-loan-duration: uint,
    timestamp: uint
  }
)

;; User Performance History
(define-map user-performance
  { user: principal, period: uint }
  {
    loans-taken: uint,
    loans-repaid: uint,
    loans-defaulted: uint,
    total-borrowed: uint,
    total-repaid: uint,
    performance-score: uint,
    risk-level: (string-ascii 10),
    last-activity: uint
  }
)

;; Market Trends Tracking
(define-map market-trends
  uint ;; block-period
  {
    avg-collateral-ratio: uint,
    popular-duration: uint,
    demand-supply-ratio: uint,
    risk-premium: uint,
    market-sentiment: (string-ascii 10)
  }
)

;; Risk Assessment Cache
(define-map risk-profiles
  principal
  {
    credit-score: uint,
    risk-category: (string-ascii 10),
    recommended-rate: uint,
    max-loan-amount: uint,
    last-updated: uint
  }
)

;; Performance Alerts
(define-map performance-alerts
  uint ;; alert-id
  {
    alert-type: (string-ascii 20),
    threshold-value: uint,
    current-value: uint,
    triggered-at: uint,
    severity: (string-ascii 10),
    message: (string-ascii 100)
  }
)

(define-data-var next-alert-id uint u1)

;; Read-only functions

(define-read-only (get-platform-metrics (period uint))
  (map-get? platform-metrics period)
)

(define-read-only (get-current-platform-metrics)
  (let ((current-period (/ stacks-block-height (var-get performance-window))))
    (map-get? platform-metrics current-period)
  )
)

(define-read-only (get-user-performance (user principal) (period uint))
  (map-get? user-performance { user: user, period: period })
)

(define-read-only (get-market-trends (period uint))
  (map-get? market-trends period)
)

(define-read-only (get-risk-profile (user principal))
  (map-get? risk-profiles user)
)

(define-read-only (get-performance-alert (alert-id uint))
  (map-get? performance-alerts alert-id)
)

(define-read-only (calculate-success-rate (completed uint) (defaulted uint))
  (let ((total (+ completed defaulted)))
    (if (> total u0)
      (/ (* completed u10000) total) ;; percentage * 100
      u0
    )
  )
)

;; Public functions

(define-public (record-loan-created (loan-id uint) (borrower principal) (amount uint) (interest-rate uint) (duration uint))
  (let 
    (
      (current-period (/ stacks-block-height (var-get performance-window)))
      (current-metrics (default-to 
        { total-loans: u0, active-loans: u0, completed-loans: u0, defaulted-loans: u0,
          total-volume: u0, total-interest-earned: u0, success-rate: u0,
          average-interest-rate: u0, average-loan-duration: u0, timestamp: u0 }
        (get-platform-metrics current-period)))
      (user-perf (default-to
        { loans-taken: u0, loans-repaid: u0, loans-defaulted: u0, total-borrowed: u0,
          total-repaid: u0, performance-score: u50, risk-level: "MEDIUM", last-activity: u0 }
        (get-user-performance borrower current-period)))
    )
    (asserts! (var-get analytics-enabled) (ok true))
    
    ;; Update platform metrics
    (map-set platform-metrics current-period
      (merge current-metrics {
        total-loans: (+ (get total-loans current-metrics) u1),
        active-loans: (+ (get active-loans current-metrics) u1),
        total-volume: (+ (get total-volume current-metrics) amount),
        timestamp: stacks-block-height
      })
    )
    
    ;; Update user performance
    (map-set user-performance { user: borrower, period: current-period }
      (merge user-perf {
        loans-taken: (+ (get loans-taken user-perf) u1),
        total-borrowed: (+ (get total-borrowed user-perf) amount),
        last-activity: stacks-block-height
      })
    )
    
    ;; Update risk profile
    (update-risk-profile borrower)
    
    (ok true)
  )
)

(define-public (record-loan-repaid (loan-id uint) (borrower principal) (amount uint) (interest-paid uint))
  (let 
    (
      (current-period (/ stacks-block-height (var-get performance-window)))
      (current-metrics (unwrap! (get-platform-metrics current-period) err-not-found))
      (user-perf (unwrap! (get-user-performance borrower current-period) err-not-found))
    )
    (asserts! (var-get analytics-enabled) (ok true))
    
    ;; Update platform metrics
    (map-set platform-metrics current-period
      (merge current-metrics {
        active-loans: (- (get active-loans current-metrics) u1),
        completed-loans: (+ (get completed-loans current-metrics) u1),
        total-interest-earned: (+ (get total-interest-earned current-metrics) interest-paid),
        success-rate: (calculate-success-rate 
          (+ (get completed-loans current-metrics) u1)
          (get defaulted-loans current-metrics))
      })
    )
    
    ;; Update user performance
    (map-set user-performance { user: borrower, period: current-period }
      (merge user-perf {
        loans-repaid: (+ (get loans-repaid user-perf) u1),
        total-repaid: (+ (get total-repaid user-perf) (+ amount interest-paid)),
        performance-score: (if (< (+ (get performance-score user-perf) u10) u100) (+ (get performance-score user-perf) u10) u100),
        last-activity: stacks-block-height
      })
    )
    
    (ok true)
  )
)

(define-public (record-loan-defaulted (loan-id uint) (borrower principal) (amount uint))
  (let 
    (
      (current-period (/ stacks-block-height (var-get performance-window)))
      (current-metrics (unwrap! (get-platform-metrics current-period) err-not-found))
      (user-perf (unwrap! (get-user-performance borrower current-period) err-not-found))
    )
    (asserts! (var-get analytics-enabled) (ok true))
    
    ;; Update platform metrics
    (map-set platform-metrics current-period
      (merge current-metrics {
        active-loans: (- (get active-loans current-metrics) u1),
        defaulted-loans: (+ (get defaulted-loans current-metrics) u1),
        success-rate: (calculate-success-rate 
          (get completed-loans current-metrics)
          (+ (get defaulted-loans current-metrics) u1))
      })
    )
    
    ;; Update user performance  
    (map-set user-performance { user: borrower, period: current-period }
      (merge user-perf {
        loans-defaulted: (+ (get loans-defaulted user-perf) u1),
        performance-score: (if (> (get performance-score user-perf) u20) (- (get performance-score user-perf) u20) u0),
        risk-level: "HIGH",
        last-activity: stacks-block-height
      })
    )
    
    ;; Check for performance alerts
    (try! (check-default-rate-alert current-period))
    
    (ok true)
  )
)

(define-public (update-market-trends (period uint))
  (let ((current-block stacks-block-height))
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    
    ;; This would calculate trends based on recent loan data
    (map-set market-trends period {
      avg-collateral-ratio: u150,
      popular-duration: u1440,
      demand-supply-ratio: u75,
      risk-premium: u5,
      market-sentiment: "STABLE"
    })
    
    (ok true)
  )
)

(define-public (trigger-performance-alert (alert-type (string-ascii 20)) (threshold uint) (current uint) (severity (string-ascii 10)) (message (string-ascii 100)))
  (let ((alert-id (var-get next-alert-id)))
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    
    (map-set performance-alerts alert-id {
      alert-type: alert-type,
      threshold-value: threshold,
      current-value: current,
      triggered-at: stacks-block-height,
      severity: severity,
      message: message
    })
    
    (var-set next-alert-id (+ alert-id u1))
    (ok alert-id)
  )
)

(define-public (set-analytics-config (enabled bool) (window uint) (sample-size uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (> window u0) err-invalid-period)
    (asserts! (> sample-size u0) err-invalid-threshold)
    
    (var-set analytics-enabled enabled)
    (var-set performance-window window)
    (var-set min-sample-size sample-size)
    (ok true)
  )
)

;; Private functions

(define-private (update-risk-profile (user principal))
  (let 
    (
      (current-period (/ stacks-block-height (var-get performance-window)))
      (user-perf (get-user-performance user current-period))
    )
    (match user-perf
      perf (let 
        (
          (credit-score (get performance-score perf))
          (risk-category (if (< credit-score u30) "HIGH"
                           (if (< credit-score u70) "MEDIUM" "LOW")))
          (recommended-rate (+ u5 (/ (- u100 credit-score) u10)))
          (max-amount (* credit-score u1000))
        )
        (map-set risk-profiles user {
          credit-score: credit-score,
          risk-category: risk-category,
          recommended-rate: recommended-rate,
          max-loan-amount: max-amount,
          last-updated: stacks-block-height
        })
        true
      )
      false
    )
  )
)

(define-private (check-default-rate-alert (period uint))
  (let 
    (
      (metrics (get-platform-metrics period))
      (alert-threshold u2000) ;; 20% default rate
    )
    (match metrics
      data (if (> (- u10000 (get success-rate data)) alert-threshold)
        (trigger-performance-alert 
          "HIGH_DEFAULT_RATE" 
          alert-threshold 
          (- u10000 (get success-rate data))
          "HIGH"
          "Default rate exceeded 20% threshold")
        (ok u0))
      (ok u0)
    )
  )
)
