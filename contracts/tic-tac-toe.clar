;; Tic-Tac-Toe with Lending Integration
;; Players can borrow from lending pool to play games

;; Reference to lending pool
(define-constant lending-pool-contract .lending-pool-v2)

;; Constants
(define-constant THIS_CONTRACT (as-contract tx-sender))
(define-constant ERR_MIN_BET_AMOUNT (err u100))
(define-constant ERR_INVALID_MOVE (err u101))
(define-constant ERR_GAME_NOT_FOUND (err u102))
(define-constant ERR_GAME_CANNOT_BE_JOINED (err u103))
(define-constant ERR_NOT_YOUR_TURN (err u104))
(define-constant ERR_BORROW_FAILED (err u105))
(define-constant ERR_INSUFFICIENT_FUNDS (err u106))

;; Data variables
(define-data-var latest-game-id uint u0)

;; Game structure tracking lending status
(define-map games uint {
    player-one: principal,
    player-two: (optional principal),
    is-player-one-turn: bool,
    bet-amount: uint,
    board: (list 9 uint),
    winner: (optional principal),
    player-one-borrowed: bool,
    player-o-borrowed: bool,
    player-one-borrow-id: (optional uint),
    player-o-borrow-id: (optional uint)
})

;; ===== HELPER FUNCTIONS =====

(define-private (validate-move (board (list 9 uint)) (move-index uint) (move uint))
    (let (
        (index-in-range (and (>= move-index u0) (< move-index u9)))
        (x-or-o (or (is-eq move u1) (is-eq move u2)))
        (empty-spot (is-eq (unwrap! (element-at? board move-index) false) u0))
    )
    (and (is-eq index-in-range true) (is-eq x-or-o true) empty-spot)
))

(define-private (is-line (board (list 9 uint)) (a uint) (b uint) (c uint)) 
    (let (
        (a-val (unwrap! (element-at? board a) false))
        (b-val (unwrap! (element-at? board b) false))
        (c-val (unwrap! (element-at? board c) false))
    )
    (and (is-eq a-val b-val) (is-eq a-val c-val) (not (is-eq a-val u0)))
))

(define-private (has-won (board (list 9 uint))) 
    (or
        (is-line board u0 u1 u2)
        (is-line board u3 u4 u5)
        (is-line board u6 u7 u8)
        (is-line board u0 u3 u6)
        (is-line board u1 u4 u7)
        (is-line board u2 u5 u8)
        (is-line board u0 u4 u8)
        (is-line board u2 u4 u6)
    )
)

;; ===== GAME CREATION =====

;; Traditional game creation (no lending)
(define-public (create-game (bet-amount uint) (move-index uint) (move uint))
    (let (
        (game-id (var-get latest-game-id))
        (starting-board (list u0 u0 u0 u0 u0 u0 u0 u0 u0))
        (game-board (unwrap! (replace-at? starting-board move-index move) ERR_INVALID_MOVE))
        (game-data {
            player-one: contract-caller,
            player-two: none,
            is-player-one-turn: false,
            bet-amount: bet-amount,
            board: game-board,
            winner: none,
            player-one-borrowed: false,
            player-o-borrowed: false,
            player-one-borrow-id: none,
            player-o-borrow-id: none
        })
    )
    (asserts! (> bet-amount u0) ERR_MIN_BET_AMOUNT)
    (asserts! (is-eq move u1) ERR_INVALID_MOVE)
    (asserts! (validate-move starting-board move-index move) ERR_INVALID_MOVE)

    (try! (stx-transfer? bet-amount contract-caller THIS_CONTRACT))
    (map-set games game-id game-data)
    (var-set latest-game-id (+ game-id u1))

    (print { action: "create-game", data: game-data})
    (ok game-id)
))

;; Create game with lending
(define-public (create-game-with-borrow (bet-amount uint) (move-index uint) (move uint) (use-lending bool))
    (let (
        (game-id (var-get latest-game-id))
        (starting-board (list u0 u0 u0 u0 u0 u0 u0 u0 u0))
        (game-board (unwrap! (replace-at? starting-board move-index move) ERR_INVALID_MOVE))
    )
    (asserts! (> bet-amount u0) ERR_MIN_BET_AMOUNT)
    (asserts! (is-eq move u1) ERR_INVALID_MOVE)
    (asserts! (validate-move starting-board move-index move) ERR_INVALID_MOVE)

    (if use-lending
        ;; Borrow from lending pool
        (let (
            (borrow-id (unwrap! (contract-call? lending-pool-contract borrow-for-game bet-amount contract-caller) ERR_BORROW_FAILED))
        )
            ;; Borrowed STX is sent to player, now stake it into game
            (try! (stx-transfer? bet-amount contract-caller THIS_CONTRACT))
            
            (map-set games game-id {
                player-one: contract-caller,
                player-two: none,
                is-player-one-turn: false,
                bet-amount: bet-amount,
                board: game-board,
                winner: none,
                player-one-borrowed: true,
                player-o-borrowed: false,
                player-one-borrow-id: (some borrow-id),
                player-o-borrow-id: none
            })
            (var-set latest-game-id (+ game-id u1))
            (print { action: "create-game-with-borrow", data: { game-id: game-id, borrow-id: borrow-id }})
            (ok game-id)
        )
        ;; Traditional game creation
        (begin
            (try! (stx-transfer? bet-amount contract-caller THIS_CONTRACT))
            (map-set games game-id {
                player-one: contract-caller,
                player-two: none,
                is-player-one-turn: false,
                bet-amount: bet-amount,
                board: game-board,
                winner: none,
                player-one-borrowed: false,
                player-o-borrowed: false,
                player-one-borrow-id: none,
                player-o-borrow-id: none
            })
            (var-set latest-game-id (+ game-id u1))
            (print { action: "create-game", data: game-id})
            (ok game-id)
        )
    )
))

;; ===== JOIN GAME =====

;; Traditional join (no lending)
(define-public (join-game (game-id uint) (move-index uint) (move uint))
    (let (
        (original-game-data (unwrap! (map-get? games game-id) ERR_GAME_NOT_FOUND))
        (original-board (get board original-game-data))
        (game-board (unwrap! (replace-at? original-board move-index move) ERR_INVALID_MOVE))
        (game-data (merge original-game-data {
            board: game-board,
            player-two: (some contract-caller),
            is-player-one-turn: true
        }))
    )
    (asserts! (is-none (get player-two original-game-data)) ERR_GAME_CANNOT_BE_JOINED) 
    (asserts! (is-eq move u2) ERR_INVALID_MOVE)
    (asserts! (validate-move original-board move-index move) ERR_INVALID_MOVE)

    (try! (stx-transfer? (get bet-amount original-game-data) contract-caller THIS_CONTRACT))
    (map-set games game-id game-data)

    (print { action: "join-game", data: game-data})
    (ok game-id)
))

;; Join game with lending
(define-public (join-game-with-borrow (game-id uint) (move-index uint) (move uint) (use-lending bool))
    (let (
        (original-game-data (unwrap! (map-get? games game-id) ERR_GAME_NOT_FOUND))
        (original-board (get board original-game-data))
        (bet-amount (get bet-amount original-game-data))
        (game-board (unwrap! (replace-at? original-board move-index move) ERR_INVALID_MOVE))
    )
    (asserts! (is-none (get player-two original-game-data)) ERR_GAME_CANNOT_BE_JOINED)
    (asserts! (is-eq move u2) ERR_INVALID_MOVE)
    (asserts! (validate-move original-board move-index move) ERR_INVALID_MOVE)

    (if use-lending
        ;; Borrow from lending pool
        (let (
            (borrow-id (unwrap! (contract-call? lending-pool-contract borrow-for-game bet-amount contract-caller) ERR_BORROW_FAILED))
        )
            (try! (stx-transfer? bet-amount contract-caller THIS_CONTRACT))
            (map-set games game-id (merge original-game-data {
                board: game-board,
                player-two: (some contract-caller),
                is-player-one-turn: true,
                player-o-borrowed: true,
                player-o-borrow-id: (some borrow-id)
            }))
            (print { action: "join-game-with-borrow", data: { game-id: game-id, borrow-id: borrow-id }})
            (ok game-id)
        )
        ;; Traditional join
        (begin
            (try! (stx-transfer? bet-amount contract-caller THIS_CONTRACT))
            (map-set games game-id (merge original-game-data {
                board: game-board,
                player-two: (some contract-caller),
                is-player-one-turn: true
            }))
            (print { action: "join-game", data: game-id})
            (ok game-id)
        )
    )
))

;; ===== PLAY GAME =====

(define-public (play (game-id uint) (move-index uint) (move uint))
    (let (
        (original-game-data (unwrap! (map-get? games game-id) ERR_GAME_NOT_FOUND))
        (original-board (get board original-game-data))
        (is-player-one-turn (get is-player-one-turn original-game-data))
        (player-turn (if is-player-one-turn (get player-one original-game-data) (unwrap! (get player-two original-game-data) ERR_GAME_NOT_FOUND)))
        (expected-move (if is-player-one-turn u1 u2))
        (game-board (unwrap! (replace-at? original-board move-index move) ERR_INVALID_MOVE))
        (is-now-winner (has-won game-board))
        (game-data (merge original-game-data {
            board: game-board,
            is-player-one-turn: (not is-player-one-turn),
            winner: (if is-now-winner (some player-turn) none)
        }))
    )
    (asserts! (is-eq player-turn contract-caller) ERR_NOT_YOUR_TURN)
    (asserts! (is-eq move expected-move) ERR_INVALID_MOVE)
    (asserts! (validate-move original-board move-index move) ERR_INVALID_MOVE)

    ;; If game won, handle payouts and loan repayments
    (if is-now-winner
        (let (
            (winner player-turn)
            (loser (if is-player-one-turn 
                      (unwrap! (get player-two original-game-data) ERR_GAME_NOT_FOUND)
                      (get player-one original-game-data)))
            (winner-is-player-one (is-eq winner (get player-one original-game-data)))
            (winner-borrowed (if winner-is-player-one 
                                (get player-one-borrowed original-game-data)
                                (get player-o-borrowed original-game-data)))
            (winner-borrow-id (if winner-is-player-one 
                                 (get player-one-borrow-id original-game-data)
                                 (get player-o-borrow-id original-game-data)))
            (total-pot (* u2 (get bet-amount game-data)))
        )
            ;; If winner borrowed, repay loan from pot
            (if winner-borrowed
                (match winner-borrow-id
                    borrow-id
                   (let (
    (borrow-details-opt (unwrap! (contract-call? lending-pool-contract get-borrow-details borrow-id) ERR_BORROW_FAILED))
    (borrow-details (unwrap! borrow-details-opt ERR_BORROW_FAILED))
    (repayment-amount (+ (get amount borrow-details) (get interest-owed borrow-details)))
    (winner-payout (- total-pot repayment-amount))
)
                        ;; Repay loan from contract
                        (try! (as-contract (contract-call? lending-pool-contract repay-borrow borrow-id winner)))
                        ;; Send remaining to winner
                        (try! (as-contract (stx-transfer? winner-payout tx-sender winner)))
                        true
                    )
                    false
                )
                ;; No loan, send full pot to winner
                (try! (as-contract (stx-transfer? total-pot tx-sender winner)))
            )
            true
        )
        false
    )

    (map-set games game-id game-data)
    (print {action: "play", data: game-data})
    (ok game-id)
))

;; ===== READ-ONLY FUNCTIONS =====

(define-read-only (get-game (game-id uint))
    (map-get? games game-id)
)

(define-read-only (get-latest-game-id)
    (var-get latest-game-id)
)