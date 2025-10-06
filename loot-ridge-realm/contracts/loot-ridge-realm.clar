;; Loot Ridge Realm - Cross-Chain Tournament Platform
;; A comprehensive smart contract for managing tournaments, player profiles, and achievements

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-tournament-full (err u104))
(define-constant err-tournament-active (err u105))
(define-constant err-unauthorized (err u106))
(define-constant err-invalid-params (err u107))
(define-constant err-tournament-ended (err u108))

;; Data Variables
(define-data-var tournament-counter uint u0)
(define-data-var profile-counter uint u0)
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee

;; Data Maps

;; Competitor Profiles (NFT-based)
(define-map competitor-profiles
    { profile-id: uint }
    {
        owner: principal,
        username: (string-ascii 50),
        total-tournaments: uint,
        tournaments-won: uint,
        total-earnings: uint,
        skill-rating: uint, ;; ELO-style rating, starts at 1000
        created-at: uint
    }
)

;; Profile ownership mapping
(define-map profile-owners
    { owner: principal }
    { profile-id: uint }
)

;; Tournament Structure
(define-map tournaments
    { tournament-id: uint }
    {
        creator: principal,
        game-name: (string-ascii 100),
        entry-fee: uint,
        prize-pool: uint,
        max-participants: uint,
        current-participants: uint,
        status: (string-ascii 20), ;; "open", "active", "completed", "cancelled"
        winner: (optional principal),
        start-block: uint,
        end-block: uint
    }
)

;; Tournament Participants
(define-map tournament-participants
    { tournament-id: uint, participant: principal }
    {
        profile-id: uint,
        entry-paid: bool,
        placement: uint,
        skill-rating-at-entry: uint
    }
)

;; Achievement Records (Cross-chain verifiable)
(define-map achievements
    { profile-id: uint, achievement-id: uint }
    {
        game-name: (string-ascii 100),
        achievement-type: (string-ascii 50),
        verified: bool,
        timestamp: uint,
        metadata-hash: (buff 32) ;; Hash of achievement data for verification
    }
)

(define-map achievement-counters
    { profile-id: uint }
    { count: uint }
)

;; Read-only functions

(define-read-only (get-profile (profile-id uint))
    (map-get? competitor-profiles { profile-id: profile-id })
)

(define-read-only (get-profile-by-owner (owner principal))
    (match (map-get? profile-owners { owner: owner })
        profile-data (get-profile (get profile-id profile-data))
        none
    )
)

(define-read-only (get-tournament (tournament-id uint))
    (map-get? tournaments { tournament-id: tournament-id })
)

(define-read-only (get-participant-info (tournament-id uint) (participant principal))
    (map-get? tournament-participants { tournament-id: tournament-id, participant: participant })
)

(define-read-only (get-achievement (profile-id uint) (achievement-id uint))
    (map-get? achievements { profile-id: profile-id, achievement-id: achievement-id })
)

(define-read-only (get-platform-fee)
    (var-get platform-fee-percentage)
)

(define-read-only (calculate-prize-distribution (prize-pool uint))
    (let
        (
            (platform-fee (/ (* prize-pool (var-get platform-fee-percentage)) u100))
            (winner-share (/ (* (- prize-pool platform-fee) u60) u100))
            (runner-up-share (/ (* (- prize-pool platform-fee) u30) u100))
            (third-place-share (- (- prize-pool platform-fee) (+ winner-share runner-up-share)))
        )
        {
            platform-fee: platform-fee,
            winner: winner-share,
            runner-up: runner-up-share,
            third-place: third-place-share
        }
    )
)

;; Public functions

;; Create Competitor Profile (NFT)
(define-public (create-profile (username (string-ascii 50)))
    (let
        (
            (new-profile-id (+ (var-get profile-counter) u1))
            (caller tx-sender)
        )
        ;; Check if user already has a profile
        (asserts! (is-none (map-get? profile-owners { owner: caller })) err-already-exists)
        
        ;; Create profile
        (map-set competitor-profiles
            { profile-id: new-profile-id }
            {
                owner: caller,
                username: username,
                total-tournaments: u0,
                tournaments-won: u0,
                total-earnings: u0,
                skill-rating: u1000, ;; Starting ELO rating
                created-at: block-height
            }
        )
        
        ;; Map owner to profile
        (map-set profile-owners
            { owner: caller }
            { profile-id: new-profile-id }
        )
        
        ;; Initialize achievement counter
        (map-set achievement-counters
            { profile-id: new-profile-id }
            { count: u0 }
        )
        
        (var-set profile-counter new-profile-id)
        (ok new-profile-id)
    )
)

;; Create Tournament
(define-public (create-tournament 
    (game-name (string-ascii 100))
    (entry-fee uint)
    (max-participants uint)
    (duration-blocks uint))
    (let
        (
            (new-tournament-id (+ (var-get tournament-counter) u1))
            (caller tx-sender)
        )
        ;; Validate parameters
        (asserts! (> max-participants u1) err-invalid-params)
        (asserts! (> duration-blocks u0) err-invalid-params)
        
        ;; Create tournament
        (map-set tournaments
            { tournament-id: new-tournament-id }
            {
                creator: caller,
                game-name: game-name,
                entry-fee: entry-fee,
                prize-pool: u0,
                max-participants: max-participants,
                current-participants: u0,
                status: "open",
                winner: none,
                start-block: block-height,
                end-block: (+ block-height duration-blocks)
            }
        )
        
        (var-set tournament-counter new-tournament-id)
        (ok new-tournament-id)
    )
)

;; Join Tournament
(define-public (join-tournament (tournament-id uint))
    (let
        (
            (caller tx-sender)
            (tournament (unwrap! (get-tournament tournament-id) err-not-found))
            (profile-data (unwrap! (get-profile-by-owner caller) err-not-found))
            (profile-id (unwrap! (get profile-id (map-get? profile-owners { owner: caller })) err-not-found))
            (entry-fee (get entry-fee tournament))
        )
        ;; Validate tournament state
        (asserts! (is-eq (get status tournament) "open") err-tournament-active)
        (asserts! (< (get current-participants tournament) (get max-participants tournament)) err-tournament-full)
        (asserts! (is-none (get-participant-info tournament-id caller)) err-already-exists)
        
        ;; Transfer entry fee if required
        (if (> entry-fee u0)
            (try! (stx-transfer? entry-fee caller (as-contract tx-sender)))
            true
        )
        
        ;; Add participant
        (map-set tournament-participants
            { tournament-id: tournament-id, participant: caller }
            {
                profile-id: profile-id,
                entry-paid: true,
                placement: u0,
                skill-rating-at-entry: (get skill-rating profile-data)
            }
        )
        
        ;; Update tournament
        (map-set tournaments
            { tournament-id: tournament-id }
            (merge tournament {
                current-participants: (+ (get current-participants tournament) u1),
                prize-pool: (+ (get prize-pool tournament) entry-fee)
            })
        )
        
        ;; Update profile tournament count
        (map-set competitor-profiles
            { profile-id: profile-id }
            (merge profile-data {
                total-tournaments: (+ (get total-tournaments profile-data) u1)
            })
        )
        
        (ok true)
    )
)

;; Complete Tournament and Distribute Prizes
(define-public (complete-tournament 
    (tournament-id uint)
    (winner principal)
    (runner-up (optional principal))
    (third-place (optional principal)))
    (let
        (
            (tournament (unwrap! (get-tournament tournament-id) err-not-found))
            (caller tx-sender)
            (prize-pool (get prize-pool tournament))
            (distribution (calculate-prize-distribution prize-pool))
            (winner-profile-data (unwrap! (get-profile-by-owner winner) err-not-found))
            (winner-profile-id (unwrap! (get profile-id (map-get? profile-owners { owner: winner })) err-not-found))
        )
        ;; Only creator or contract owner can complete tournament
        (asserts! (or (is-eq caller (get creator tournament)) (is-eq caller contract-owner)) err-unauthorized)
        (asserts! (is-eq (get status tournament) "open") err-tournament-ended)
        
        ;; Update tournament status
        (map-set tournaments
            { tournament-id: tournament-id }
            (merge tournament {
                status: "completed",
                winner: (some winner)
            })
        )
        
        ;; Distribute prizes
        (if (> prize-pool u0)
            (begin
                ;; Pay winner
                (try! (as-contract (stx-transfer? (get winner distribution) tx-sender winner)))
                
                ;; Pay runner-up if exists
                (match runner-up
                    runner (try! (as-contract (stx-transfer? (get runner-up distribution) tx-sender runner)))
                    true
                )
                
                ;; Pay third place if exists
                (match third-place
                    third (try! (as-contract (stx-transfer? (get third-place distribution) tx-sender third)))
                    true
                )
                
                ;; Platform fee stays in contract
                true
            )
            true
        )
        
        ;; Update winner's profile
        (map-set competitor-profiles
            { profile-id: winner-profile-id }
            (merge winner-profile-data {
                tournaments-won: (+ (get tournaments-won winner-profile-data) u1),
                total-earnings: (+ (get total-earnings winner-profile-data) (get winner distribution)),
                skill-rating: (+ (get skill-rating winner-profile-data) u50) ;; ELO increase
            })
        )
        
        (ok true)
    )
)

;; Add Achievement (Cross-chain verification placeholder)
(define-public (add-achievement 
    (profile-id uint)
    (game-name (string-ascii 100))
    (achievement-type (string-ascii 50))
    (metadata-hash (buff 32)))
    (let
        (
            (profile (unwrap! (get-profile profile-id) err-not-found))
            (caller tx-sender)
            (counter-data (unwrap! (map-get? achievement-counters { profile-id: profile-id }) err-not-found))
            (new-achievement-id (+ (get count counter-data) u1))
        )
        ;; Only profile owner can add achievements
        (asserts! (is-eq caller (get owner profile)) err-unauthorized)
        
        ;; Add achievement
        (map-set achievements
            { profile-id: profile-id, achievement-id: new-achievement-id }
            {
                game-name: game-name,
                achievement-type: achievement-type,
                verified: false, ;; Requires verification
                timestamp: block-height,
                metadata-hash: metadata-hash
            }
        )
        
        ;; Update counter
        (map-set achievement-counters
            { profile-id: profile-id }
            { count: new-achievement-id }
        )
        
        (ok new-achievement-id)
    )
)

;; Verify Achievement (Admin/Oracle function)
(define-public (verify-achievement (profile-id uint) (achievement-id uint))
    (let
        (
            (achievement (unwrap! (get-achievement profile-id achievement-id) err-not-found))
        )
        ;; Only contract owner can verify (in production, this would be an oracle)
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set achievements
            { profile-id: profile-id, achievement-id: achievement-id }
            (merge achievement { verified: true })
        )
        
        (ok true)
    )
)

;; Update Skill Rating (Admin function for cross-game adjustments)
(define-public (update-skill-rating (profile-id uint) (new-rating uint))
    (let
        (
            (profile (unwrap! (get-profile profile-id) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set competitor-profiles
            { profile-id: profile-id }
            (merge profile { skill-rating: new-rating })
        )
        
        (ok true)
    )
)

;; Withdraw platform fees (Owner only)
(define-public (withdraw-fees (amount uint) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (as-contract (stx-transfer? amount tx-sender recipient))
    )
)

;; Update platform fee percentage (Owner only)
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u20) err-invalid-params) ;; Max 20% fee
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)