;; Data structures for positions
(define-map positions
  { trader: principal }
  {
    size: int,              ;; positive for long, negative for short
    entry-price: uint,      ;; price at which position was opened
    collateral: uint,       ;; amount of collateral posted
    liquidation-price: uint,;; price at which position gets liquidated
    last-funding-time: uint ;; last time funding was paid/received
  }
)

;; Market state
(define-map market-state
  { market: (string-ascii 10) }  ;; e.g., "BTC-USD"
  {
    open-interest-long: uint,   ;; total long positions
    open-interest-short: uint,  ;; total short positions
    funding-rate: int,          ;; current funding rate (can be positive or negative)
    last-price: uint,           ;; last known price from UTXOs
    liquidity-pool: uint        ;; available liquidity for trades
  }
)

;; Constants
(define-constant MAX-LEVERAGE u10)         ;; 10x max leverage
(define-constant FUNDING-INTERVAL u28800)  ;; 8 hours in blocks
(define-constant MIN-COLLATERAL u100000)   ;; minimum collateral required
(define-constant LIQUIDATION-THRESHOLD u80) ;; liquidate at 80% collateral

;; Open a long position
(define-public (open-long (size uint) (collateral uint))
  (let (
    (leverage (/ (* size (get-current-price)) collateral))
    (liquidation-price (calculate-liquidation-price true size collateral))
  )
    (asserts! (<= leverage MAX-LEVERAGE) ERR-MAX-LEVERAGE)
    (asserts! (>= collateral MIN-COLLATERAL) ERR-MIN-COLLATERAL)
    
    ;; Transfer collateral
    (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
    
    ;; Create position
    (map-set positions 
      { trader: tx-sender }
      {
        size: size,
        entry-price: (get-current-price),
        collateral: collateral,
        liquidation-price: liquidation-price,
        last-funding-time: block-height
      }
    )
    (ok true)
  )
)

;; Open a short position 
(define-public (open-short (size uint) (collateral uint))
  (let (
    (leverage (/ (* size (get-current-price)) collateral))
    (liquidation-price (calculate-liquidation-price false size collateral))
  )
    (asserts! (<= leverage MAX-LEVERAGE) ERR-MAX-LEVERAGE)
    (asserts! (>= collateral MIN-COLLATERAL) ERR-MIN-COLLATERAL)
    
    ;; Transfer collateral
    (try! (stx-transfer? collateral tx-sender (as-contract tx-sender)))
    
    ;; Create position
    (map-set positions 
      { trader: tx-sender }
      {
        size: (* size -1), ;; negative for shorts
        entry-price: (get-current-price),
        collateral: collateral,
        liquidation-price: liquidation-price,
        last-funding-time: block-height
      }
    )
    (ok true)
  )
)

;; Calculate funding payment
(define-private (calculate-funding-payment (position { size: int, last-funding-time: uint }))
  (let (
    (funding-rate (get funding-rate (map-get? market-state { market: "BTC-USD" })))
    (elapsed-blocks (- block-height last-funding-time))
    (position-value (* (abs size) (get-current-price)))
  )
    ;; Funding amount = position value * funding rate * time elapsed
    (* position-value (* funding-rate elapsed-blocks))
  )
)

;; Settle funding payment
(define-public (settle-funding)
  (let (
    (trader tx-sender)
    (position (unwrap! (map-get? positions { trader: tx-sender }) ERR-NO-POSITION))
    (funding-payment (calculate-funding-payment position))
  )
    (if (> funding-payment 0)
      ;; If positive, trader receives funding
      (as-contract (stx-transfer? funding-payment tx-sender trader))
      ;; If negative, trader pays funding
      (stx-transfer? (abs funding-payment) trader (as-contract tx-sender))
    )
  )
)

;; Liquidate position if necessary
(define-public (liquidate (trader principal))
  (let (
    (position (unwrap! (map-get? positions { trader: trader }) ERR-NO-POSITION))
    (current-price (get-current-price))
  )
    (asserts! (is-liquidatable position current-price) ERR-NOT-LIQUIDATABLE)
    
    ;; Handle liquidation
    (map-delete positions { trader: trader })
    ;; Transfer remaining collateral after fees
    (ok true)
  )
)