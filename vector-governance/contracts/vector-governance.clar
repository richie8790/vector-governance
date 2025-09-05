;; VectorGovernance - Enhanced Multi-Dimensional Reputation-Based Decision Platform
;; A decentralized governance system with reputation-weighted voting and STX-backed proposals

;; =============================================================================
;; ERROR CONSTANTS
;; =============================================================================
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INVALID-AMOUNT (err u1001))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u1002))
(define-constant ERR-ALREADY-EXECUTED (err u1003))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u1004))
(define-constant ERR-ORGANIZATION-NOT-FOUND (err u1005))
(define-constant ERR-NOT-MEMBER (err u1006))
(define-constant ERR-INVALID-INPUT (err u1007))
(define-constant ERR-ALREADY-CLAIMED (err u1008))
(define-constant ERR-SELF-VOTE-PROHIBITED (err u1009))
(define-constant ERR-ALREADY-MEMBER (err u1010))
(define-constant ERR-PROPOSAL-EXPIRED (err u1011))
(define-constant ERR-INSUFFICIENT-QUORUM (err u1012))
(define-constant ERR-ORGANIZATION-INACTIVE (err u1013))
(define-constant ERR-VOTING-PERIOD_ENDED (err u1014))

;; =============================================================================
;; SYSTEM CONSTANTS
;; =============================================================================
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-PROPOSAL-AMOUNT u1000000000) ;; 1,000 STX (in micro-STX)
(define-constant MIN-PROPOSAL-AMOUNT u1000000) ;; 1 STX minimum
(define-constant MIN-REPUTATION-TO-PROPOSE u50)
(define-constant MIN-REPUTATION-TO_VOTE u10)

;; Reputation system constants
(define-constant FOUNDER-INITIAL-REPUTATION u1000)
(define-constant MEMBER-INITIAL-REPUTATION u100)
(define-constant REPUTATION-PER-MICROSTX u10) ;; Reputation gained per micro-STX voted
(define-constant PROPOSAL-CREATION-REPUTATION u25)
(define-constant SUCCESSFUL-PROPOSAL-BONUS u100)

;; Governance parameters
(define-constant VOTING-PERIOD-BLOCKS u1440) ;; ~10 days at 10 min/block
(define-constant QUORUM-PERCENTAGE u25) ;; 25% of members must participate
(define-constant EXECUTION-THRESHOLD-PERCENTAGE u60) ;; 60% approval needed

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================
(define-data-var next-proposal-id uint u1)
(define-data-var next-organization-id uint u1)
(define-data-var platform-fee-rate uint u250) ;; 2.5% fee (250 basis points)
(define-data-var total-platform-fees uint u0)

;; =============================================================================
;; DATA MAPS
;; =============================================================================

;; Organization registry
(define-map organizations
    { org-id: uint }
    {
        name: (string-ascii 64),
        description: (string-ascii 256),
        founder: principal,
        member-count: uint,
        total-proposals: uint,
        successful-proposals: uint,
        total-volume: uint,
        is-active: bool,
        created-at: uint,
        min-reputation-to-propose: uint,
        quorum-threshold: uint
    }
)

;; Member profiles with enhanced tracking
(define-map members
    { org-id: uint, member: principal }
    {
        reputation: uint,
        votes-cast: uint,
        stx-voted: uint,
        proposals-created: uint,
        successful-proposals: uint,
        joined-at: uint,
        last-active: uint,
        is-active: bool
    }
)

;; Proposal registry with enhanced metadata
(define-map proposals
    { proposal-id: uint }
    {
        org-id: uint,
        proposer: principal,
        title: (string-ascii 128),
        description: (string-ascii 512),
        funding-amount: uint,
        votes-for: uint,
        votes-against: uint,
        total-stx-voted: uint,
        unique-voters: uint,
        created-at: uint,
        voting-ends-at: uint,
        executed: bool,
        claimed: bool,
        execution-threshold: uint,
        category: (string-ascii 32)
    }
)

;; Vote tracking with support/opposition
(define-map votes
    { proposal-id: uint, voter: principal }
    {
        stx-amount: uint,
        support: bool, ;; true = for, false = against
        voted-at: uint,
        reputation-at-vote: uint
    }
)

;; Member vote totals per proposal (for quorum calculation)
(define-map vote-totals
    { proposal-id: uint, voter: principal }
    { total-stx: uint }
)

;; Organization member list for efficient querying
(define-map org-members
    { org-id: uint, member-index: uint }
    { member: principal }
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

(define-read-only (get-organization (org-id uint))
    (map-get? organizations { org-id: org-id })
)

(define-read-only (get-member (org-id uint) (member principal))
    (map-get? members { org-id: org-id, member: member })
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-vote-total (proposal-id uint) (voter principal))
    (default-to 
        { total-stx: u0 }
        (map-get? vote-totals { proposal-id: proposal-id, voter: voter })
    )
)

(define-read-only (is-member (org-id uint) (member principal))
    (match (get-member org-id member)
        member-data (get is-active member-data)
        false
    )
)

(define-read-only (get-member-reputation (org-id uint) (member principal))
    (match (get-member org-id member)
        member-data (some (get reputation member-data))
        none
    )
)

(define-read-only (can-propose (org-id uint) (member principal))
    (match (get-organization org-id)
        org-data
        (match (get-member org-id member)
            member-data
            (and 
                (get is-active org-data)
                (get is-active member-data)
                (>= (get reputation member-data) (get min-reputation-to-propose org-data))
            )
            false
        )
        false
    )
)

(define-read-only (can-vote (org-id uint) (member principal))
    (match (get-member org-id member)
        member-data
        (and 
            (get is-active member-data)
            (>= (get reputation member-data) MIN-REPUTATION-TO_VOTE)
        )
        false
    )
)

(define-read-only (get-proposal-status (proposal-id uint))
    (match (get-proposal proposal-id)
        proposal-data
        (let (
            (current-block block-height)
            (voting-ended (> current-block (get voting-ends-at proposal-data)))
            (execution-met (>= (get votes-for proposal-data) (get execution-threshold proposal-data)))
            (quorum-met (>= (get unique-voters proposal-data) 
                           (get quorum-threshold 
                                (unwrap-panic (get-organization (get org-id proposal-data))))))
        )
        (some {
            proposal-id: proposal-id,
            voting-active: (not voting-ended),
            quorum-met: quorum-met,
            execution-threshold-met: execution-met,
            can-execute: (and voting-ended quorum-met execution-met),
            executed: (get executed proposal-data),
            claimed: (get claimed proposal-data)
        }))
        none
    )
)

(define-read-only (get-platform-stats)
    {
        total-organizations: (- (var-get next-organization-id) u1),
        total-proposals: (- (var-get next-proposal-id) u1),
        platform-fee-rate: (var-get platform-fee-rate),
        total-fees-collected: (var-get total-platform-fees)
    }
)

;; =============================================================================
;; PRIVATE FUNCTIONS
;; =============================================================================

(define-private (validate-member (org-id uint) (member principal))
    (if (is-member org-id member)
        (ok true)
        ERR-NOT-MEMBER
    )
)

(define-private (validate-organization (org-id uint))
    (match (get-organization org-id)
        org-data
        (if (get is-active org-data)
            (ok org-data)
            ERR-ORGANIZATION-INACTIVE
        )
        ERR-ORGANIZATION-NOT-FOUND
    )
)

(define-private (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-rate)) u10000)
)

(define-private (update-member-reputation (org-id uint) (member principal) (points uint))
    (match (get-member org-id member)
        member-data
        (begin
            (map-set members
                { org-id: org-id, member: member }
                (merge member-data { 
                    reputation: (+ (get reputation member-data) points),
                    last-active: block-height
                })
            )
            (ok true)
        )
        ERR-NOT-MEMBER
    )
)

(define-private (is-voting-period-active (proposal-id uint))
    (match (get-proposal proposal-id)
        proposal-data
        (<= block-height (get voting-ends-at proposal-data))
        false
    )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - ORGANIZATION MANAGEMENT
;; =============================================================================

(define-public (create-organization 
    (name (string-ascii 64))
    (description (string-ascii 256))
    (min-reputation-to-propose uint)
    (quorum-threshold uint))
    (let (
        (org-id (var-get next-organization-id))
    )
        ;; Validation
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        (asserts! (> (len description) u0) ERR-INVALID-INPUT)
        (asserts! (>= min-reputation-to-propose MIN-REPUTATION-TO-PROPOSE) ERR-INVALID-INPUT)
        (asserts! (and (>= quorum-threshold u1) (<= quorum-threshold u100)) ERR-INVALID-INPUT)
        
        ;; Create organization
        (map-set organizations
            { org-id: org-id }
            {
                name: name,
                description: description,
                founder: tx-sender,
                member-count: u1,
                total-proposals: u0,
                successful-proposals: u0,
                total-volume: u0,
                is-active: true,
                created-at: block-height,
                min-reputation-to-propose: min-reputation-to-propose,
                quorum-threshold: quorum-threshold
            }
        )
        
        ;; Add founder as first member with high reputation
        (map-set members
            { org-id: org-id, member: tx-sender }
            {
                reputation: FOUNDER-INITIAL-REPUTATION,
                votes-cast: u0,
                stx-voted: u0,
                proposals-created: u0,
                successful-proposals: u0,
                joined-at: block-height,
                last-active: block-height,
                is-active: true
            }
        )
        
        ;; Add to member list
        (map-set org-members
            { org-id: org-id, member-index: u0 }
            { member: tx-sender }
        )
        
        (var-set next-organization-id (+ org-id u1))
        (ok org-id)
    )
)

(define-public (join-organization (org-id uint))
    (let (
        (org-data (try! (validate-organization org-id)))
        (current-member-count (get member-count org-data))
    )
        (asserts! (not (is-member org-id tx-sender)) ERR-ALREADY-MEMBER)
        
        ;; Add new member
        (map-set members
            { org-id: org-id, member: tx-sender }
            {
                reputation: MEMBER-INITIAL-REPUTATION,
                votes-cast: u0,
                stx-voted: u0,
                proposals-created: u0,
                successful-proposals: u0,
                joined-at: block-height,
                last-active: block-height,
                is-active: true
            }
        )
        
        ;; Add to member list
        (map-set org-members
            { org-id: org-id, member-index: current-member-count }
            { member: tx-sender }
        )
        
        ;; Update organization member count
        (map-set organizations
            { org-id: org-id }
            (merge org-data { 
                member-count: (+ current-member-count u1)
            })
        )
        
        (ok true)
    )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - PROPOSAL MANAGEMENT
;; =============================================================================

(define-public (create-proposal 
    (org-id uint) 
    (title (string-ascii 128)) 
    (description (string-ascii 512))
    (funding-amount uint)
    (category (string-ascii 32)))
    (let (
        (proposal-id (var-get next-proposal-id))
        (org-data (try! (validate-organization org-id)))
        (member-data (unwrap! (get-member org-id tx-sender) ERR-NOT-MEMBER))
        (execution-threshold (/ (* funding-amount EXECUTION-THRESHOLD-PERCENTAGE) u100))
    )
        ;; Validation
        (asserts! (> (len title) u0) ERR-INVALID-INPUT)
        (asserts! (> (len description) u0) ERR-INVALID-INPUT)
        (asserts! (> (len category) u0) ERR-INVALID-INPUT)
        (asserts! (and (>= funding-amount MIN-PROPOSAL-AMOUNT) 
                      (<= funding-amount MAX-PROPOSAL-AMOUNT)) ERR-INVALID-AMOUNT)
        (asserts! (can-propose org-id tx-sender) ERR-INSUFFICIENT-REPUTATION)
        
        ;; Create proposal
        (map-set proposals
            { proposal-id: proposal-id }
            {
                org-id: org-id,
                proposer: tx-sender,
                title: title,
                description: description,
                funding-amount: funding-amount,
                votes-for: u0,
                votes-against: u0,
                total-stx-voted: u0,
                unique-voters: u0,
                created-at: block-height,
                voting-ends-at: (+ block-height VOTING-PERIOD-BLOCKS),
                executed: false,
                claimed: false,
                execution-threshold: execution-threshold,
                category: category
            }
        )
        
        ;; Update member stats
        (map-set members
            { org-id: org-id, member: tx-sender }
            (merge member-data { 
                proposals-created: (+ (get proposals-created member-data) u1)
            })
        )
        
        ;; Update organization stats
        (map-set organizations
            { org-id: org-id }
            (merge org-data { 
                total-proposals: (+ (get total-proposals org-data) u1)
            })
        )
        
        ;; Award reputation for creating proposal
        (try! (update-member-reputation org-id tx-sender PROPOSAL-CREATION-REPUTATION))
        
        (var-set next-proposal-id (+ proposal-id u1))
        (ok proposal-id)
    )
)

(define-public (vote-on-proposal (proposal-id uint) (stx-amount uint) (support bool))
    (let (
        (proposal-data (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (org-id (get org-id proposal-data))
        (member-data (unwrap! (get-member org-id tx-sender) ERR-NOT-MEMBER))
        (existing-vote (get-vote proposal-id tx-sender))
        (platform-fee (calculate-platform-fee stx-amount))
        (vote-amount (- stx-amount platform-fee))
        (is-new-voter (is-none existing-vote))
    )
        ;; Validation
        (asserts! (> stx-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (not (is-eq tx-sender (get proposer proposal-data))) ERR-SELF-VOTE-PROHIBITED)
        (asserts! (can-vote org-id tx-sender) ERR-INSUFFICIENT-REPUTATION)
        (asserts! (is-voting-period-active proposal-id) ERR-VOTING-PERIOD_ENDED)
        (asserts! (not (get executed proposal-data)) ERR-ALREADY-EXECUTED)
        
        ;; Transfer STX (including platform fee)
        (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
        
        ;; Update platform fees
        (var-set total-platform-fees (+ (var-get total-platform-fees) platform-fee))
        
        ;; Record vote
        (map-set votes
            { proposal-id: proposal-id, voter: tx-sender }
            {
                stx-amount: vote-amount,
                support: support,
                voted-at: block-height,
                reputation-at-vote: (get reputation member-data)
            }
        )
        
        ;; Update vote totals
        (let (
            (current-total (get total-stx (get-vote-total proposal-id tx-sender)))
        )
            (map-set vote-totals
                { proposal-id: proposal-id, voter: tx-sender }
                { total-stx: (+ current-total vote-amount) }
            )
        )
        
        ;; Update proposal vote counts
        (let (
            (new-votes-for (if support 
                              (+ (get votes-for proposal-data) vote-amount)
                              (get votes-for proposal-data)))
            (new-votes-against (if support 
                                  (get votes-against proposal-data)
                                  (+ (get votes-against proposal-data) vote-amount)))
            (new-unique-voters (if is-new-voter
                                  (+ (get unique-voters proposal-data) u1)
                                  (get unique-voters proposal-data)))
        )
            (map-set proposals
                { proposal-id: proposal-id }
                (merge proposal-data { 
                    votes-for: new-votes-for,
                    votes-against: new-votes-against,
                    total-stx-voted: (+ (get total-stx-voted proposal-data) vote-amount),
                    unique-voters: new-unique-voters
                })
            )
        )
        
        ;; Update member stats and reputation
        (let (
            (reputation-gain (/ vote-amount REPUTATION-PER-MICROSTX))
        )
            (map-set members
                { org-id: org-id, member: tx-sender }
                (merge member-data { 
                    votes-cast: (+ (get votes-cast member-data) u1),
                    stx-voted: (+ (get stx-voted member-data) vote-amount),
                    reputation: (+ (get reputation member-data) reputation-gain),
                    last-active: block-height
                })
            )
        )
        
        (ok true)
    )
)

(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal-data (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (org-id (get org-id proposal-data))
        (org-data (unwrap! (get-organization org-id) ERR-ORGANIZATION-NOT-FOUND))
        (proposer (get proposer proposal-data))
        (funding-amount (get funding-amount proposal-data))
        (total-voted (get total-stx-voted proposal-data))
    )
        ;; Validation
        (asserts! (not (is-voting-period-active proposal-id)) ERR-VOTING-PERIOD_ENDED)
        (asserts! (not (get executed proposal-data)) ERR-ALREADY-EXECUTED)
        (asserts! (>= (get unique-voters proposal-data) (get quorum-threshold org-data)) ERR-INSUFFICIENT-QUORUM)
        (asserts! (>= (get votes-for proposal-data) (get execution-threshold proposal-data)) ERR-INSUFFICIENT-REPUTATION)
        
        ;; Mark as executed
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal-data { 
                executed: true,
                claimed: true ;; Auto-claim on execution
            })
        )
        
        ;; Transfer funds to proposer
        (try! (as-contract (stx-transfer? total-voted tx-sender proposer)))
        
        ;; Update organization stats
        (map-set organizations
            { org-id: org-id }
            (merge org-data { 
                successful-proposals: (+ (get successful-proposals org-data) u1),
                total-volume: (+ (get total-volume org-data) total-voted)
            })
        )
        
        ;; Award bonus reputation to successful proposer
        (try! (update-member-reputation org-id proposer SUCCESSFUL-PROPOSAL-BONUS))
        
        ;; Update proposer's successful proposal count
        (let (
            (proposer-data (unwrap! (get-member org-id proposer) ERR-NOT-MEMBER))
        )
            (map-set members
                { org-id: org-id, member: proposer }
                (merge proposer-data { 
                    successful-proposals: (+ (get successful-proposals proposer-data) u1)
                })
            )
        )
        
        (ok true)
    )
)

;; =============================================================================
;; PUBLIC FUNCTIONS - ADMINISTRATION
;; =============================================================================

(define-public (update-organization-settings 
    (org-id uint)
    (min-reputation-to-propose uint)
    (quorum-threshold uint))
    (let (
        (org-data (unwrap! (get-organization org-id) ERR-ORGANIZATION-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get founder org-data)) ERR-NOT-AUTHORIZED)
        (asserts! (>= min-reputation-to-propose MIN-REPUTATION-TO-PROPOSE) ERR-INVALID-INPUT)
        (asserts! (and (>= quorum-threshold u1) (<= quorum-threshold u100)) ERR-INVALID-INPUT)
        
        (map-set organizations
            { org-id: org-id }
            (merge org-data { 
                min-reputation-to-propose: min-reputation-to-propose,
                quorum-threshold: quorum-threshold
            })
        )
        
        (ok true)
    )
)

(define-public (toggle-organization-status (org-id uint))
    (let (
        (org-data (unwrap! (get-organization org-id) ERR-ORGANIZATION-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get founder org-data)) ERR-NOT-AUTHORIZED)
        
        (map-set organizations
            { org-id: org-id }
            (merge org-data { 
                is-active: (not (get is-active org-data))
            })
        )
        
        (ok true)
    )
)

;; Platform admin functions (only contract owner)
(define-public (update-platform-fee-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-rate u1000) ERR-INVALID-INPUT) ;; Max 10%
        (var-set platform-fee-rate new-rate)
        (ok true)
    )
)

(define-public (withdraw-platform-fees (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= amount (var-get total-platform-fees)) ERR-INVALID-AMOUNT)
        
        (var-set total-platform-fees (- (var-get total-platform-fees) amount))
        (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
        (ok true)
    )
)