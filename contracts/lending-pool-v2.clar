;; Tic Tac Toe Lending Pool Contract v2
;; Improved with share-based accounting, collateral, and better distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-insufficient-liquidity (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-unauthorized-game (err u104))
(define-constant err-game-not-found (err u105))
(define-constant err-already-repaid (err u106))
(define-constant err-paused (err u107))
(define-constant err-insufficient-collateral (err u108))
(define-constant err-loan-defaulted (err u109))
(define-constant err-no-shares (err u110))

;; Interest rate: 5% per game (500 basis points)
(define-constant interest-rate u500)
(define-constant basis-points u10000)

;; Collateral requirement: 150% of loan (borrower must stake 50% extra)
(define-constant collateral-ratio u15000) ;; 150%

;; Default time: 1000 blocks (~1 week if blocks are ~10min)
(define-constant default-block-time u1000)

;; Data Variables
(define-data-var total-pool-stx uint u0) ;; Total STX held by pool
(define-data-var total-shares uint u0) ;; Total pool shares issued
(define-data-var total-borrowed uint u0)
(define-data-var paused bool false)
(define-data-var borrow-nonce uint u0)

;; Data Maps

;; Lender shares (not direct STX balance)
(define-map lender-shares principal uint)

;; Active borrows with collateral tracking
(define-map active-borrows uint {
    amount: uint,
    borrower: principal,
    game-contract: principal,
    repaid: bool,
    interest-owed: uint,
    collateral: uint,
    borrow-block: uint,
    defaulted: bool
})

;; Authorized games
(define-map authorized-games principal bool)

;; Read-only functions

(define-read-only (get-pool-stx)
    (ok (var-get total-pool-stx))
)

(define-read-only (get-total-shares)
    (ok (var-get total-shares))
)

(define-read-only (get-available-liquidity)
    (ok (- (var-get total-pool-stx) (var-get total-borrowed)))
)

(define-read-only (get-lender-shares (lender principal))
    (ok (default-to u0 (map-get? lender-shares lender)))
)

;; Calculate STX value of shares (this grows as interest is earned)
(define-read-only (shares-to-stx (shares uint))
    (let (
        (total-s (var-get total-shares))
        (total-stx (var-get total-pool-stx))
    )
        (if (is-eq total-s u0)
            (ok shares) ;; 1:1 if no shares exist yet
            (ok (/ (* shares total-stx) total-s))
        )
    )
)

;; Calculate shares for STX amount
(define-read-only (stx-to-shares (amount uint))
    (let (
        (total-s (var-get total-shares))
        (total-stx (var-get total-pool-stx))
    )
        (if (is-eq total-stx u0)
            (ok amount) ;; 1:1 if pool is empty
            (ok (/ (* amount total-s) total-stx))
        )
    )
)

;; Get lender's STX balance (including earned interest)
(define-read-only (get-lender-stx-value (lender principal))
    (let (
        (shares (default-to u0 (map-get? lender-shares lender)))
    )
        (shares-to-stx shares)
    )
)

(define-read-only (get-total-borrowed)
    (ok (var-get total-borrowed))
)

(define-read-only (get-borrow-details (borrow-id uint))
    (ok (map-get? active-borrows borrow-id))
)

(define-read-only (is-game-authorized (game-contract principal))
    (ok (default-to false (map-get? authorized-games game-contract)))
)

(define-read-only (calculate-interest (amount uint))
    (ok (/ (* amount interest-rate) basis-points))
)

(define-read-only (calculate-required-collateral (amount uint))
    (ok (/ (* amount collateral-ratio) basis-points))
)

;; Public functions

;; Deposit STX and receive shares
(define-public (deposit (amount uint))
    (let (
        (current-shares (default-to u0 (map-get? lender-shares tx-sender)))
        (new-shares (unwrap! (stx-to-shares amount) err-invalid-amount))
    )
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (not (var-get paused)) err-paused)
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set lender-shares tx-sender (+ current-shares new-shares))
        (var-set total-shares (+ (var-get total-shares) new-shares))
        (var-set total-pool-stx (+ (var-get total-pool-stx) amount))
        
        (ok new-shares)
    )
)

;; Withdraw STX by burning shares
(define-public (withdraw (amount uint))
    (let (
        (lender-share-balance (default-to u0 (map-get? lender-shares tx-sender)))
        (shares-needed (unwrap! (stx-to-shares amount) err-invalid-amount))
        (available (- (var-get total-pool-stx) (var-get total-borrowed)))
        (actual-stx-value (unwrap! (shares-to-stx shares-needed) err-invalid-amount))
    )
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= lender-share-balance shares-needed) err-insufficient-balance)
        (asserts! (>= available actual-stx-value) err-insufficient-liquidity)
        
        (try! (as-contract (stx-transfer? actual-stx-value tx-sender tx-sender)))
        
        (map-set lender-shares tx-sender (- lender-share-balance shares-needed))
        (var-set total-shares (- (var-get total-shares) shares-needed))
        (var-set total-pool-stx (- (var-get total-pool-stx) actual-stx-value))
        
        (ok actual-stx-value)
    )
)

;; Borrow funds with collateral (called by authorized game contract)
(define-public (borrow-for-game (amount uint) (borrower principal))
    (let (
        (available (- (var-get total-pool-stx) (var-get total-borrowed)))
        (interest (unwrap! (calculate-interest amount) err-invalid-amount))
        (required-collateral (unwrap! (calculate-required-collateral amount) err-invalid-amount))
        (borrow-id (var-get borrow-nonce))
    )
        (asserts! (default-to false (map-get? authorized-games contract-caller)) err-unauthorized-game)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= available amount) err-insufficient-liquidity)
        (asserts! (not (var-get paused)) err-paused)
        
        ;; Borrower must provide collateral
        (try! (stx-transfer? required-collateral borrower (as-contract tx-sender)))
        
        ;; Transfer loan amount to borrower
        (try! (as-contract (stx-transfer? amount tx-sender borrower)))
        
        (map-set active-borrows borrow-id {
            amount: amount,
            borrower: borrower,
            game-contract: contract-caller,
            repaid: false,
            interest-owed: interest,
            collateral: required-collateral,
            borrow-block: stacks-block-height,
            defaulted: false
        })
        
        (var-set total-borrowed (+ (var-get total-borrowed) amount))
        (var-set borrow-nonce (+ borrow-id u1))
        
        (ok borrow-id)
    )
)

;; Repay borrowed amount with interest, get collateral back
(define-public (repay-borrow (borrow-id uint) (payer principal))
    (let (
        (borrow-info (unwrap! (map-get? active-borrows borrow-id) err-game-not-found))
        (total-repayment (+ (get amount borrow-info) (get interest-owed borrow-info)))
    )
        (asserts! (is-eq contract-caller (get game-contract borrow-info)) err-unauthorized-game)
        (asserts! (not (get repaid borrow-info)) err-already-repaid)
        (asserts! (not (get defaulted borrow-info)) err-loan-defaulted)
        
        ;; Receive repayment from payer
        (try! (stx-transfer? total-repayment payer (as-contract tx-sender)))
        
        ;; Return collateral to borrower
        (try! (as-contract (stx-transfer? (get collateral borrow-info) tx-sender (get borrower borrow-info))))
        
        ;; Update state - interest goes into pool, increasing share value
        (map-set active-borrows borrow-id (merge borrow-info { repaid: true }))
        (var-set total-borrowed (- (var-get total-borrowed) (get amount borrow-info)))
        (var-set total-pool-stx (+ (var-get total-pool-stx) total-repayment))
        
        (ok true)
    )
)

;; Liquidate defaulted loan (anyone can call after default time)
(define-public (liquidate-loan (borrow-id uint))
    (let (
        (borrow-info (unwrap! (map-get? active-borrows borrow-id) err-game-not-found))
        (blocks-passed (- stacks-block-height (get borrow-block borrow-info)))
    )
        (asserts! (not (get repaid borrow-info)) err-already-repaid)
        (asserts! (not (get defaulted borrow-info)) err-loan-defaulted)
        (asserts! (>= blocks-passed default-block-time) (err u111))
        
        ;; Mark as defaulted and keep collateral in pool
        (map-set active-borrows borrow-id (merge borrow-info { defaulted: true }))
        (var-set total-borrowed (- (var-get total-borrowed) (get amount borrow-info)))
        ;; Collateral stays in pool as compensation (it's already in the contract)
        ;; This partially covers the loss
        (var-set total-pool-stx (+ (var-get total-pool-stx) (get collateral borrow-info)))
        
        (ok true)
    )
)

;; Admin functions

(define-public (authorize-game (game-contract principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-games game-contract true)
        (ok true)
    )
)

(define-public (revoke-game (game-contract principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set authorized-games game-contract false)
        (ok true)
    )
)

(define-public (set-paused (pause bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set paused pause)
        (ok true)
    )
)