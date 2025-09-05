;; VectorGovernance - Multi-Dimensional Reputation-Based Decision Platform

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-EXECUTED (err u103))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u104))
(define-constant ERR-ORGANIZATION-NOT-FOUND (err u105))
(define-constant ERR-NOT-MEMBER (err u106))
(define-constant ERR-INVALID-INPUT (err u107))
(define-constant ERR-ALREADY-CLAIMED (err u108))
(define-constant ERR-CANNOT-VOTE-ON-OWN-PROPOSAL (err u109))
(define-constant ERR-ALREADY-MEMBER (err u110))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-PROPOSAL-WEIGHT u100000000) ;; 100 STX (in micro-STX)
(define-constant MIN-VECTOR-SCORE u10)
(define-constant INITIAL-VECTOR-SCORE u100)
(define-constant NEW-MEMBER-VECTOR-SCORE u50)
(define-constant VECTOR-POINTS-PER-MICROSTX u100) ;; 1 vector point per 100 micro-STX

;; Data Variables
(define-data-var next-proposal-id uint u1)
(define-data-var next-organization-id uint u1)

;; Data Maps
(define-map Organizations
    { organization-id: uint }
    {
        name: (string-ascii 50),
        founder: principal,
        member-count: uint,
        total-decisions: uint,
        is-active: bool,
        created-at: uint
    }
)

(define-map VectorMembers
    { organization-id: uint, member: principal }
    {
        reputation: uint,
        votes-cast: uint,
        proposals-made: uint,
        joined-at: uint
    }
)

(define-map GovernanceProposals
    { proposal-id: uint }
    {
        organization-id: uint,
        proposer: principal,
        title: (string-ascii 100),
        amount: uint,
        executed: bool,
        claimed: bool,
        created-at: uint,
        total-voting-weight: uint
    }
)

(define-map VotingActions
    { proposal-id: uint, voter: principal }
    {
        amount: uint,
        voted-at: uint
    }
)

(define-map VoterTotals
    { proposal-id: uint, voter: principal }
    { total-amount: uint }
)

;; Read-only Functions
(define-read-only (get-organization (organization-id uint))
    (map-get? Organizations { organization-id: organization-id })
)

(define-read-only (get-vector-member (organization-id uint) (member principal))
    (map-get? VectorMembers { organization-id: organization-id, member: member })
)

(define-read-only (get-governance-proposal (proposal-id uint))
    (map-get? GovernanceProposals { proposal-id: proposal-id })
)

(define-read-only (get-voting-action (proposal-id uint) (voter principal))
    (map-get? VotingActions { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-voter-total (proposal-id uint) (voter principal))
    (default-to 
        { total-amount: u0 }
        (map-get? VoterTotals { proposal-id: proposal-id, voter: voter })
    )
)

(define-read-only (is-vector-member (organization-id uint) (member principal))
    (is-some (map-get? VectorMembers { organization-id: organization-id, member: member }))
)

(define-read-only (get-next-proposal-id)
    (var-get next-proposal-id)
)

(define-read-only (get-next-organization-id)
    (var-get next-organization-id)
)

;; Private Functions
(define-private (validate-vector-member (organization-id uint) (member principal))
    (if (is-vector-member organization-id member)
        (ok true)
        ERR-NOT-MEMBER
    )
)

(define-private (validate-organization-exists (organization-id uint))
    (let (
        (organization-opt (get-organization organization-id))
    )
        (if (is-some organization-opt)
            (let (
                (organization-data (unwrap-panic organization-opt))
            )
                (if (get is-active organization-data)
                    (ok organization-data)
                    ERR-ORGANIZATION-NOT-FOUND)
            )
            ERR-ORGANIZATION-NOT-FOUND
        )
    )
)

(define-private (update-member-vector-score (organization-id uint) (member principal) (points uint))
    (let (
        (member-opt (get-vector-member organization-id member))
    )
        (if (is-some member-opt)
            (let (
                (member-data (unwrap-panic member-opt))
            )
                (map-set VectorMembers
                    { organization-id: organization-id, member: member }
                    (merge member-data { 
                        reputation: (+ (get reputation member-data) points)
                    })
                )
                (ok true)
            )
            ERR-NOT-MEMBER
        )
    )
)

;; Public Functions

;; Create a new organization
(define-public (create-organization (name (string-ascii 50)))
    (let (
        (organization-id (var-get next-organization-id))
    )
        (asserts! (> (len name) u0) ERR-INVALID-INPUT)
        (asserts! (<= (len name) u50) ERR-INVALID-INPUT)
        
        (map-set Organizations
            { organization-id: organization-id }
            {
                name: name,
                founder: tx-sender,
                member-count: u1,
                total-decisions: u0,
                is-active: true,
                created-at: block-height
            }
        )
        
        (map-set VectorMembers
            { organization-id: organization-id, member: tx-sender }
            {
                reputation: INITIAL-VECTOR-SCORE,
                votes-cast: u0,
                proposals-made: u0,
                joined-at: block-height
            }
        )
        
        (var-set next-organization-id (+ organization-id u1))
        (ok organization-id)
    )
)

;; Join an existing organization
(define-public (join-organization (organization-id uint))
    (let (
        (organization-data (try! (validate-organization-exists organization-id)))
    )
        (asserts! (not (is-vector-member organization-id tx-sender)) ERR-ALREADY-MEMBER)
        
        (map-set VectorMembers
            { organization-id: organization-id, member: tx-sender }
            {
                reputation: NEW-MEMBER-VECTOR-SCORE,
                votes-cast: u0,
                proposals-made: u0,
                joined-at: block-height
            }
        )
        
        (map-set Organizations
            { organization-id: organization-id }
            (merge organization-data { 
                member-count: (+ (get member-count organization-data) u1)
            })
        )
        
        (ok true)
    )
)

;; Submit a governance proposal
(define-public (submit-proposal 
    (organization-id uint) 
    (title (string-ascii 100)) 
    (amount uint))
    (let (
        (proposal-id (var-get next-proposal-id))
        (member-data (unwrap! (get-vector-member organization-id tx-sender) ERR-NOT-MEMBER))
    )
        (asserts! (> (len title) u0) ERR-INVALID-INPUT)
        (asserts! (<= (len title) u100) ERR-INVALID-INPUT)
        (asserts! (and (> amount u0) (<= amount MAX-PROPOSAL-WEIGHT)) ERR-INVALID-AMOUNT)
        (try! (validate-organization-exists organization-id))
        (try! (validate-vector-member organization-id tx-sender))
        (asserts! (>= (get reputation member-data) MIN-VECTOR-SCORE) ERR-NOT-AUTHORIZED)
        
        (map-set GovernanceProposals
            { proposal-id: proposal-id }
            {
                organization-id: organization-id,
                proposer: tx-sender,
                title: title,
                amount: amount,
                executed: false,
                claimed: false,
                created-at: block-height,
                total-voting-weight: u0
            }
        )
        
        (map-set VectorMembers
            { organization-id: organization-id, member: tx-sender }
            (merge member-data { 
                proposals-made: (+ (get proposals-made member-data) u1)
            })
        )
        
        (var-set next-proposal-id (+ proposal-id u1))
        (ok proposal-id)
    )
)

;; Cast vote on a proposal
(define-public (cast-vote (proposal-id uint) (amount uint))
    (let (
        (proposal-data (unwrap! (get-governance-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (current-total (get total-voting-weight proposal-data))
        (voter-current (get total-amount (get-voter-total proposal-id tx-sender)))
        (organization-id (get organization-id proposal-data))
    )
        (asserts! (not (get executed proposal-data)) ERR-ALREADY-EXECUTED)
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (not (is-eq tx-sender (get proposer proposal-data))) ERR-CANNOT-VOTE-ON-OWN-PROPOSAL)
        (try! (validate-vector-member organization-id tx-sender))
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update voting action record
        (map-set VotingActions
            { proposal-id: proposal-id, voter: tx-sender }
            {
                amount: amount,
                voted-at: block-height
            }
        )
        
        ;; Update voter's total votes for this proposal
        (map-set VoterTotals
            { proposal-id: proposal-id, voter: tx-sender }
            { total-amount: (+ voter-current amount) }
        )
        
        ;; Update proposal total
        (let (
            (new-total (+ current-total amount))
            (is-now-executed (>= new-total (get amount proposal-data)))
        )
            (map-set GovernanceProposals
                { proposal-id: proposal-id }
                (merge proposal-data { 
                    total-voting-weight: new-total,
                    executed: is-now-executed
                })
            )
        )
        
        ;; Update voter vector score
        (try! (update-member-vector-score 
            organization-id 
            tx-sender 
            (/ amount VECTOR-POINTS-PER-MICROSTX)))
        
        ;; Update voter's vote count
        (let (
            (member-data (unwrap! (get-vector-member organization-id tx-sender) ERR-NOT-MEMBER))
        )
            (map-set VectorMembers
                { organization-id: organization-id, member: tx-sender }
                (merge member-data { 
                    votes-cast: (+ (get votes-cast member-data) u1)
                })
            )
        )
        
        (ok true)
    )
)

;; Claim executed proposal funds (only proposer can call)
(define-public (claim-proposal (proposal-id uint))
    (let (
        (proposal-data (unwrap! (get-governance-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
        (organization-id (get organization-id proposal-data))
    )
        (asserts! (is-eq tx-sender (get proposer proposal-data)) ERR-NOT-AUTHORIZED)
        (asserts! (get executed proposal-data) ERR-INSUFFICIENT-VOTING-POWER)
        (asserts! (not (get claimed proposal-data)) ERR-ALREADY-CLAIMED)
        
        ;; Mark as claimed first to prevent re-entrancy
        (map-set GovernanceProposals
            { proposal-id: proposal-id }
            (merge proposal-data { claimed: true })
        )
        
        ;; Transfer funds to proposer
        (try! (as-contract (stx-transfer? (get total-voting-weight proposal-data) tx-sender (get proposer proposal-data))))
        
        ;; Update organization total decisions
        (let (
            (organization-data (unwrap! (get-organization organization-id) ERR-ORGANIZATION-NOT-FOUND))
        )
            (map-set Organizations
                { organization-id: organization-id }
                (merge organization-data { 
                    total-decisions: (+ (get total-decisions organization-data) u1)
                })
            )
        )
        
        (ok true)
    )
)

;; Emergency function to deactivate organization (only founder can call)
(define-public (deactivate-organization (organization-id uint))
    (let (
        (organization-data (unwrap! (get-organization organization-id) ERR-ORGANIZATION-NOT-FOUND))
    )
        (asserts! (is-eq tx-sender (get founder organization-data)) ERR-NOT-AUTHORIZED)
        
        (map-set Organizations
            { organization-id: organization-id }
            (merge organization-data { is-active: false })
        )
        
        (ok true)
    )
)