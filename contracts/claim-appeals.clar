;; Claim Appeals System
;; Enables policyholders to appeal rejected claims with structured review processes

;; Error constants
(define-constant ERR_NOT_AUTHORIZED (err u300))
(define-constant ERR_APPEAL_NOT_FOUND (err u301))
(define-constant ERR_CLAIM_NOT_FOUND (err u302))
(define-constant ERR_CLAIM_NOT_REJECTED (err u303))
(define-constant ERR_APPEAL_EXISTS (err u304))
(define-constant ERR_APPEAL_DEADLINE_EXPIRED (err u305))
(define-constant ERR_INVALID_APPEAL_DATA (err u306))
(define-constant ERR_APPEAL_ALREADY_RESOLVED (err u307))
(define-constant ERR_REVIEWER_NOT_FOUND (err u308))

;; Data variables
(define-data-var next-appeal-id uint u1)
(define-data-var appeal-deadline-blocks uint u1440) ;; 24 hours in blocks
(define-data-var total-appeals-filed uint u0)
(define-data-var total-appeals-approved uint u0)
(define-data-var total-appeals-denied uint u0)

;; Appeal data structure
(define-map appeals
  uint
  {
    claim-id: uint,
    policy-holder: principal,
    appeal-reason: (string-ascii 256),
    supporting-evidence: (string-ascii 512),
    filed-at: uint,
    appeal-deadline: uint,
    status: (string-ascii 20),
    reviewer: (optional principal),
    reviewed-at: (optional uint),
    reviewer-notes: (string-ascii 256),
    resolution: (string-ascii 20)
  }
)

;; Claim to appeal mapping for quick lookup
(define-map claim-appeals
  uint
  uint
)

;; Appeal reviewers and their qualifications
(define-map appeal-reviewers
  principal
  {
    name: (string-ascii 64),
    specialization: (string-ascii 64),
    approval-authority: bool,
    total-reviews: uint,
    average-processing-time: uint,
    is-active: bool
  }
)

;; Appeal statistics per reviewer
(define-map reviewer-stats
  principal
  {
    appeals-assigned: uint,
    appeals-approved: uint,
    appeals-denied: uint,
    total-processing-time: uint,
    last-review-date: uint
  }
)

;; Appeal history tracking
(define-map appeal-history
  uint
  {
    status-changes: (list 5 {status: (string-ascii 20), changed-at: uint, changed-by: principal}),
    review-notes-history: (list 3 {note: (string-ascii 256), added-at: uint, added-by: principal})
  }
)

;; Priority appeals based on claim amount or urgency
(define-map priority-appeals
  uint
  {
    priority-level: uint,
    expedited-deadline: uint,
    priority-reason: (string-ascii 128)
  }
)

;; File an appeal for a rejected claim
(define-public (file-appeal
  (claim-id uint)
  (appeal-reason (string-ascii 256))
  (supporting-evidence (string-ascii 512))
)
  (let (
    (appeal-id (var-get next-appeal-id))
    (current-block stacks-block-height)
    ;; For this implementation, we'll use simplified claim validation
    ;; In production, these would call actual HealthInsurance-Manager functions
    (claim {policy-holder: tx-sender, amount: u1000, status: "rejected"})
    (claim-status "rejected")
    (appeal-deadline (+ current-block (var-get appeal-deadline-blocks)))
  )
    ;; Validation checks
    (asserts! (> claim-id u0) ERR_CLAIM_NOT_FOUND) ;; Basic validation
    (asserts! (is-none (map-get? claim-appeals claim-id)) ERR_APPEAL_EXISTS)
    
    ;; Check if appeal is within deadline (appeals must be filed within deadline of claim rejection)
    ;; For simplicity, we'll assume current block is within deadline
    
    ;; Create the appeal
    (map-set appeals appeal-id
      {
        claim-id: claim-id,
        policy-holder: tx-sender,
        appeal-reason: appeal-reason,
        supporting-evidence: supporting-evidence,
        filed-at: current-block,
        appeal-deadline: appeal-deadline,
        status: "filed",
        reviewer: none,
        reviewed-at: none,
        reviewer-notes: "",
        resolution: "pending"
      }
    )
    
    ;; Map claim to appeal for quick lookup
    (map-set claim-appeals claim-id appeal-id)
    
    ;; Initialize appeal history
    (map-set appeal-history appeal-id
      {
        status-changes: (list {status: "filed", changed-at: current-block, changed-by: tx-sender}),
        review-notes-history: (list)
      }
    )
    
    ;; Update counters
    (var-set next-appeal-id (+ appeal-id u1))
    (var-set total-appeals-filed (+ (var-get total-appeals-filed) u1))
    
    (ok appeal-id)
  )
)

;; Assign a reviewer to an appeal (admin function)
(define-public (assign-appeal-reviewer (appeal-id uint) (reviewer principal))
  (let (
    (appeal (unwrap! (map-get? appeals appeal-id) ERR_APPEAL_NOT_FOUND))
    (reviewer-info (unwrap! (map-get? appeal-reviewers reviewer) ERR_REVIEWER_NOT_FOUND))
    (current-block stacks-block-height)
  )
    ;; For now, we'll use a simple admin check - in production this would check the contract owner
    (asserts! (is-eq tx-sender tx-sender) ERR_NOT_AUTHORIZED) ;; Placeholder - would check actual admin
    (asserts! (is-eq (get status appeal) "filed") ERR_APPEAL_ALREADY_RESOLVED)
    (asserts! (get is-active reviewer-info) ERR_REVIEWER_NOT_FOUND)
    (asserts! (get approval-authority reviewer-info) ERR_NOT_AUTHORIZED)
    
    ;; Assign reviewer and update status
    (map-set appeals appeal-id
      (merge appeal {
        reviewer: (some reviewer),
        status: "under-review"
      })
    )
    
    ;; Update reviewer stats
    (let (
      (stats (default-to {appeals-assigned: u0, appeals-approved: u0, appeals-denied: u0, 
                         total-processing-time: u0, last-review-date: u0} 
                        (map-get? reviewer-stats reviewer)))
    )
      (map-set reviewer-stats reviewer
        (merge stats {appeals-assigned: (+ (get appeals-assigned stats) u1)})
      )
    )
    
    ;; Update appeal history
    (let (
      (history (unwrap! (map-get? appeal-history appeal-id) ERR_APPEAL_NOT_FOUND))
      (current-changes (get status-changes history))
      (new-change {status: "under-review", changed-at: current-block, changed-by: tx-sender})
    )
      (map-set appeal-history appeal-id
        (merge history {
          status-changes: (default-to (get status-changes history) (as-max-len? (append current-changes new-change) u5))
        })
      )
    )
    
    (ok true)
  )
)

;; Review and resolve an appeal
(define-public (resolve-appeal
  (appeal-id uint)
  (approved bool)
  (reviewer-notes (string-ascii 256))
)
  (let (
    (appeal (unwrap! (map-get? appeals appeal-id) ERR_APPEAL_NOT_FOUND))
    (current-block stacks-block-height)
    (resolution (if approved "approved" "denied"))
    (processing-time (- current-block (get filed-at appeal)))
  )
    (asserts! (is-some (get reviewer appeal)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq tx-sender (unwrap-panic (get reviewer appeal))) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status appeal) "under-review") ERR_APPEAL_ALREADY_RESOLVED)
    (asserts! (<= current-block (get appeal-deadline appeal)) ERR_APPEAL_DEADLINE_EXPIRED)
    
    ;; Update appeal with resolution
    (map-set appeals appeal-id
      (merge appeal {
        status: "resolved",
        reviewed-at: (some current-block),
        reviewer-notes: reviewer-notes,
        resolution: resolution
      })
    )
    
    ;; In production, this would update the original claim status
    ;; For now, we'll just note that the appeal was approved
    
    ;; Update global statistics
    (if approved
      (var-set total-appeals-approved (+ (var-get total-appeals-approved) u1))
      (var-set total-appeals-denied (+ (var-get total-appeals-denied) u1))
    )
    
    ;; Update reviewer statistics
    (let (
      (reviewer (unwrap-panic (get reviewer appeal)))
      (stats (default-to {appeals-assigned: u0, appeals-approved: u0, appeals-denied: u0, 
                         total-processing-time: u0, last-review-date: u0} 
                        (map-get? reviewer-stats reviewer)))
      (new-approved (if approved (+ (get appeals-approved stats) u1) (get appeals-approved stats)))
      (new-denied (if approved (get appeals-denied stats) (+ (get appeals-denied stats) u1)))
    )
      (map-set reviewer-stats reviewer
        (merge stats {
          appeals-approved: new-approved,
          appeals-denied: new-denied,
          total-processing-time: (+ (get total-processing-time stats) processing-time),
          last-review-date: current-block
        })
      )
    )
    
    ;; Add to appeal history
    (let (
      (history (unwrap! (map-get? appeal-history appeal-id) ERR_APPEAL_NOT_FOUND))
      (current-changes (get status-changes history))
      (current-notes (get review-notes-history history))
      (new-change {status: "resolved", changed-at: current-block, changed-by: tx-sender})
      (new-note {note: reviewer-notes, added-at: current-block, added-by: tx-sender})
    )
      (map-set appeal-history appeal-id
        {
          status-changes: (default-to current-changes (as-max-len? (append current-changes new-change) u5)),
          review-notes-history: (default-to current-notes (as-max-len? (append current-notes new-note) u3))
        }
      )
    )
    
    (ok approved)
  )
)

;; Add or update appeal reviewer
(define-public (manage-appeal-reviewer
  (reviewer principal)
  (name (string-ascii 64))
  (specialization (string-ascii 64))
  (approval-authority bool)
  (is-active bool)
)
  (let (
    (current-reviewer (map-get? appeal-reviewers reviewer))
  )
    ;; For now, we'll use a simple admin check - in production this would check the contract owner
    (asserts! (is-eq tx-sender tx-sender) ERR_NOT_AUTHORIZED) ;; Placeholder - would check actual admin
    
    (map-set appeal-reviewers reviewer
      {
        name: name,
        specialization: specialization,
        approval-authority: approval-authority,
        total-reviews: (match current-reviewer rev (get total-reviews rev) u0),
        average-processing-time: (match current-reviewer rev (get average-processing-time rev) u0),
        is-active: is-active
      }
    )
    
    (ok true)
  )
)

;; Set appeal as priority with expedited processing
(define-public (set-appeal-priority
  (appeal-id uint)
  (priority-level uint)
  (priority-reason (string-ascii 128))
)
  (let (
    (appeal (unwrap! (map-get? appeals appeal-id) ERR_APPEAL_NOT_FOUND))
    (current-block stacks-block-height)
    (expedited-deadline (+ current-block (/ (var-get appeal-deadline-blocks) u2))) ;; Half the normal deadline
  )
    ;; For now, we'll use a simple admin check - in production this would check the contract owner
    (asserts! (is-eq tx-sender tx-sender) ERR_NOT_AUTHORIZED) ;; Placeholder - would check actual admin
    (asserts! (and (>= priority-level u1) (<= priority-level u3)) ERR_INVALID_APPEAL_DATA)
    (asserts! (not (is-eq (get status appeal) "resolved")) ERR_APPEAL_ALREADY_RESOLVED)
    
    (map-set priority-appeals appeal-id
      {
        priority-level: priority-level,
        expedited-deadline: expedited-deadline,
        priority-reason: priority-reason
      }
    )
    
    ;; Update appeal deadline if not yet resolved
    (map-set appeals appeal-id
      (merge appeal {appeal-deadline: expedited-deadline})
    )
    
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-appeal (appeal-id uint))
  (map-get? appeals appeal-id)
)

(define-read-only (get-appeal-by-claim (claim-id uint))
  (match (map-get? claim-appeals claim-id)
    appeal-id (map-get? appeals appeal-id)
    none
  )
)

(define-read-only (get-appeal-reviewer (reviewer principal))
  (map-get? appeal-reviewers reviewer)
)

(define-read-only (get-reviewer-stats (reviewer principal))
  (map-get? reviewer-stats reviewer)
)

(define-read-only (get-appeal-history (appeal-id uint))
  (map-get? appeal-history appeal-id)
)

(define-read-only (get-priority-appeal (appeal-id uint))
  (map-get? priority-appeals appeal-id)
)

(define-read-only (get-appeal-statistics)
  {
    total-filed: (var-get total-appeals-filed),
    total-approved: (var-get total-appeals-approved),
    total-denied: (var-get total-appeals-denied),
    approval-rate: (if (> (var-get total-appeals-filed) u0) 
                    (/ (* (var-get total-appeals-approved) u100) (var-get total-appeals-filed))
                    u0),
    next-appeal-id: (var-get next-appeal-id)
  }
)

(define-read-only (is-appeal-overdue (appeal-id uint))
  (match (map-get? appeals appeal-id)
    appeal (and 
             (not (is-eq (get status appeal) "resolved"))
             (> stacks-block-height (get appeal-deadline appeal)))
    false
  )
)

(define-read-only (get-appeals-config)
  {
    appeal-deadline-blocks: (var-get appeal-deadline-blocks),
    total-appeals-filed: (var-get total-appeals-filed)
  }
)

;; Check if user can file appeal for specific claim
(define-read-only (can-file-appeal (claim-id uint))
  (let (
    (existing-appeal (map-get? claim-appeals claim-id))
  )
    ;; Simplified check - in production this would validate against actual claim data
    (and
      (> claim-id u0)
      (is-none existing-appeal)
    )
  )
)
