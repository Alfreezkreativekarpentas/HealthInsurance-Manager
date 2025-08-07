;; HealthInsurance-Manager
;; Insurance claim processing platform with policy verification and claim tracking

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-POLICY-EXISTS (err u101))
(define-constant ERR-POLICY-NOT-FOUND (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-CLAIM-EXISTS (err u104))
(define-constant ERR-CLAIM-NOT-FOUND (err u105))
(define-constant ERR-PROVIDER-EXISTS (err u106))
(define-constant ERR-PROVIDER-NOT-FOUND (err u107))
(define-constant ERR-HSA-NOT-FOUND (err u108))
(define-constant ERR-HSA-EXISTS (err u109))
(define-constant ERR-CONTRIBUTION-LIMIT-EXCEEDED (err u110))
(define-constant ERR-INSUFFICIENT-HSA-BALANCE (err u111))
(define-constant ERR-INVALID-HSA-AMOUNT (err u112))
(define-constant ERR-HSA-NOT-ELIGIBLE (err u113))



(define-constant ERR-AUTO-APPROVAL-DISABLED (err u114))
(define-constant ERR-INVALID-THRESHOLD (err u115))

(define-data-var auto-approval-enabled bool true)
(define-data-var auto-approval-amount-threshold uint u1000)
(define-data-var auto-approval-claim-history-limit uint u3)

;; Additional error constants
(define-constant ERR-APPEAL-EXISTS (err u110))
(define-constant ERR-APPEAL-NOT-FOUND (err u111))
(define-constant ERR-CLAIM-NOT-REJECTED (err u112))
(define-constant ERR-APPEAL-DEADLINE-EXPIRED (err u113))

;; Data variables for appeals
(define-data-var total-appeals uint u0)
(define-data-var appeal-deadline-blocks uint u1440)

;; Appeals data map
(define-map Appeals
    uint
    {
        claim-id: uint,
        policy-holder: principal,
        appeal-reason: (string-ascii 100),
        appeal-date: uint,
        status: (string-ascii 10),
        review-date: uint,
        reviewer-notes: (string-ascii 100)
    }
)

;; Map claim ID to appeal ID for quick lookup
(define-map ClaimAppeals
    uint
    uint
)

(define-map TrustedProviders
    principal
    {
        trust-score: uint,
        auto-approval-eligible: bool
    }
)

(define-map AutoApprovalStats
    principal
    {
        total-auto-approved: uint,
        total-auto-approved-amount: uint,
        last-auto-approval: uint
    }
)

;; Data Variables
(define-data-var insurance-token-price uint u100)
(define-data-var total-policies uint u0)
(define-data-var total-claims uint u0)

;; HSA Data Variables
(define-data-var hsa-annual-contribution-limit uint u3650)
(define-data-var hsa-catch-up-contribution-limit uint u1000)
(define-data-var hsa-annual-interest-rate uint u250)
(define-data-var total-hsa-accounts uint u0)

;; Data Maps
(define-map Policies
    principal
    {
        policy-id: uint,
        coverage-amount: uint,
        start-date: uint,
        end-date: uint,
        status: (string-ascii 10)
    }
)

(define-map Claims
    uint 
    {
        policy-holder: principal,
        amount: uint,
        provider: principal,
        date: uint,
        status: (string-ascii 10),
        description: (string-ascii 50)
    }
)

(define-map HealthcareProviders
    principal
    {
        name: (string-ascii 50),
        status: (string-ascii 10),
        joining-date: uint
    }
)

;; HSA Account Management
(define-map HSAAccounts
    principal
    {
        balance: uint,
        annual-contributions: uint,
        contribution-year: uint,
        last-interest-calculation: uint,
        account-creation-date: uint,
        is-catch-up-eligible: bool
    }
)

(define-map HSATransactions
    uint
    {
        account-holder: principal,
        transaction-type: (string-ascii 15),
        amount: uint,
        date: uint,
        description: (string-ascii 100),
        claim-id: (optional uint)
    }
)

(define-data-var total-hsa-transactions uint u0)

;; Public Functions

;; Create new insurance policy
(define-public (create-policy (coverage-amount uint) (duration uint))
    (let
        (
            (policy-id (+ (var-get total-policies) u1))
            (current-time stacks-block-height)
            (end-time (+ stacks-block-height duration))
        )
        (asserts! (> coverage-amount u0) ERR-INVALID-AMOUNT)
        (asserts! (is-none (map-get? Policies tx-sender)) ERR-POLICY-EXISTS)
        
        (map-set Policies tx-sender
            {
                policy-id: policy-id,
                coverage-amount: coverage-amount,
                start-date: current-time,
                end-date: end-time,
                status: "active"
            }
        )
        (var-set total-policies policy-id)
        (ok policy-id)
    )
)

;; File insurance claim
(define-public (file-claim (amount uint) (provider principal) (description (string-ascii 50)))
    (let
        (
            (policy (unwrap! (map-get? Policies tx-sender) ERR-POLICY-NOT-FOUND))
            (claim-id (+ (var-get total-claims) u1))
        )
        (asserts! (<= amount (get coverage-amount policy)) ERR-INVALID-AMOUNT)
        (asserts! (is-some (map-get? HealthcareProviders provider)) ERR-PROVIDER-NOT-FOUND)
        
        (map-set Claims claim-id
            {
                policy-holder: tx-sender,
                amount: amount,
                provider: provider,
                date: stacks-block-height,
                status: "pending",
                description: description
            }
        )
        (var-set total-claims claim-id)
        (ok claim-id)
    )
)

;; Register healthcare provider
(define-public (register-provider (provider principal) (name (string-ascii 50)))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? HealthcareProviders provider)) ERR-PROVIDER-EXISTS)
        
        (map-set HealthcareProviders provider
            {
                name: name,
                status: "active",
                joining-date: stacks-block-height
            }
        )
        (ok true)
    )
)

;; Process claim (only contract owner)
(define-public (process-claim (claim-id uint) (approved bool))
    (let
        (
            (claim (unwrap! (map-get? Claims claim-id) ERR-CLAIM-NOT-FOUND))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (map-set Claims claim-id
            (merge claim {status: (if approved "approved" "rejected")})
        )
        (ok true)
    )
)

;; Read-only Functions

;; Get policy details
(define-read-only (get-policy (holder principal))
    (map-get? Policies holder)
)

;; Get claim details
(define-read-only (get-claim (claim-id uint))
    (map-get? Claims claim-id)
)

;; Get provider details
(define-read-only (get-provider (provider principal))
    (map-get? HealthcareProviders provider)
)

;; Check if policy is active
(define-read-only (is-policy-active (holder principal))
    (match (map-get? Policies holder)
        policy (and 
            (is-eq (get status policy) "active")
            (>= (get end-date policy) stacks-block-height)
        )
        false
    )
)

;; Get total active policies
(define-read-only (get-total-policies)
    (var-get total-policies)
)

;; Get total claims
(define-read-only (get-total-claims)
    (var-get total-claims)
)


(define-data-var base-premium-rate uint u10)
(define-data-var age-factor uint u2)

(define-public (calculate-premium (age uint) (coverage-amount uint))
    (let
        (
            (base-amount (var-get base-premium-rate))
            (age-multiplier (* (var-get age-factor) age))
            (coverage-factor (/ coverage-amount u1000))
            (total-premium (+ (* base-amount coverage-factor) age-multiplier))
        )
        (ok total-premium)
    )
)



(define-public (renew-policy (duration uint))
    (let
        (
            (policy (unwrap! (map-get? Policies tx-sender) ERR-POLICY-NOT-FOUND))
            (current-time stacks-block-height)
            (new-end-date (+ current-time duration))
        )
        (map-set Policies tx-sender
            (merge policy 
                {
                    end-date: new-end-date,
                    status: "active"
                }
            )
        )
        (ok true)
    )
)


(define-map ClaimHistory
    principal
    (list 10 uint)
)

(define-public (add-to-claim-history (claim-id uint))
    (let
        (
            (current-history (default-to (list ) (map-get? ClaimHistory tx-sender)))
        )
        (map-set ClaimHistory tx-sender (unwrap! (as-max-len? (append current-history claim-id) u10) ERR-INVALID-AMOUNT))
        (ok true)
    )
)

(define-read-only (get-claim-history (holder principal))
    (map-get? ClaimHistory holder)
)



(define-map FamilyMembers
    principal
    (list 5 principal)
)

(define-public (add-family-member (member principal))
    (let
        (
            (current-members (default-to (list ) (map-get? FamilyMembers tx-sender)))
        )
        (map-set FamilyMembers tx-sender (unwrap! (as-max-len? (append current-members member) u5) ERR-INVALID-AMOUNT))
        (ok true)
    )
)

(define-read-only (get-family-members (holder principal))
    (map-get? FamilyMembers holder)
)


(define-map CoverageTypes
    uint
    {
        name: (string-ascii 20),
        percentage: uint,
        max-limit: uint
    }
)

(define-public (add-coverage-type (type-id uint) (name (string-ascii 20)) (percentage uint) (max-limit uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set CoverageTypes type-id
            {
                name: name,
                percentage: percentage,
                max-limit: max-limit
            }
        )
        (ok true)
    )
)

(define-read-only (get-coverage-type (type-id uint))
    (map-get? CoverageTypes type-id)
)


(define-public (suspend-policy (holder principal))
    (let
        (
            (policy (unwrap! (map-get? Policies holder) ERR-POLICY-NOT-FOUND))
        )
        (begin
            (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
            (map-set Policies holder
                (merge policy 
                    {
                        status: "suspended"
                    }
                )
            )
            (ok true)
        )
    )
)

(define-read-only (is-policy-suspended (holder principal))
    (match (map-get? Policies holder)
        policy (is-eq (get status policy) "suspended")
        false
    )
)



(define-map DiscountTiers
    uint
    {
        claims-threshold: uint,
        discount-percentage: uint
    }
)

(define-data-var discount-enabled bool true)

(define-public (set-discount-tier (tier-id uint) (claims-threshold uint) (discount-percentage uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set DiscountTiers tier-id
            {
                claims-threshold: claims-threshold,
                discount-percentage: discount-percentage
            }
        )
        (ok true)
    )
)

(define-read-only (calculate-discounted-premium (holder principal) (base-premium uint))
    (let
        (
            (claim-count (len (default-to (list) (map-get? ClaimHistory holder))))
            (tier-1 (unwrap! (map-get? DiscountTiers u1) (ok base-premium)))
            (tier-2 (unwrap! (map-get? DiscountTiers u2) (ok base-premium)))
        )
        (if (not (var-get discount-enabled))
            (ok base-premium)
            (if (<= claim-count (get claims-threshold tier-1))
                (ok (/ (* base-premium (- u100 (get discount-percentage tier-1))) u100))
                (if (<= claim-count (get claims-threshold tier-2))
                    (ok (/ (* base-premium (- u100 (get discount-percentage tier-2))) u100))
                    (ok base-premium)
                )
            )
        )
    )
)


(define-map EmergencyContacts
    principal
    {
        contact: principal,
        relationship: (string-ascii 20),
        authorized: bool
    }
)

(define-public (set-emergency-contact (contact principal) (relationship (string-ascii 20)))
    (begin
        (asserts! (is-some (map-get? Policies tx-sender)) ERR-POLICY-NOT-FOUND)
        (map-set EmergencyContacts tx-sender
            {
                contact: contact,
                relationship: relationship,
                authorized: true
            }
        )
        (ok true)
    )
)

(define-public (emergency-file-claim (policy-holder principal) (amount uint) (provider principal) (description (string-ascii 50)))
    (let
        (
            (emergency-contact (unwrap! (map-get? EmergencyContacts policy-holder) ERR-NOT-AUTHORIZED))
            (policy (unwrap! (map-get? Policies policy-holder) ERR-POLICY-NOT-FOUND))
            (claim-id (+ (var-get total-claims) u1))
        )
        (asserts! (and (is-eq tx-sender (get contact emergency-contact)) (get authorized emergency-contact)) ERR-NOT-AUTHORIZED)
        (asserts! (<= amount (get coverage-amount policy)) ERR-INVALID-AMOUNT)
        
        (map-set Claims claim-id
            {
                policy-holder: policy-holder,
                amount: amount,
                provider: provider,
                date: stacks-block-height,
                status: "pending",
                description: description
            }
        )
        (var-set total-claims claim-id)
        (ok claim-id)
    )
)


(define-public (update-insurance-token-price (new-price uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set insurance-token-price new-price)
        (ok true)
    )
)
(define-read-only (get-insurance-token-price)
    (var-get insurance-token-price)
)
(define-read-only (get-base-premium-rate)
    (var-get base-premium-rate)
)
(define-public (update-base-premium-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set base-premium-rate new-rate)
        (ok true)
    )
)
(define-public (update-age-factor (new-factor uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set age-factor new-factor)
        (ok true)
    )
)
(define-public (toggle-discount-enabled)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set discount-enabled (not (var-get discount-enabled)))
        (ok true)
    )
)

(define-public (get-claim-status (claim-id uint))
    (let
        (
            (claim (unwrap! (map-get? Claims claim-id) ERR-CLAIM-NOT-FOUND))
        )
        (ok (get status claim))
    )
)


(define-public (set-provider-trust-status (provider principal) (trust-score uint) (auto-eligible bool))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-some (map-get? HealthcareProviders provider)) ERR-PROVIDER-NOT-FOUND)
        (map-set TrustedProviders provider
            {
                trust-score: trust-score,
                auto-approval-eligible: auto-eligible
            }
        )
        (ok true)
    )
)

(define-public (file-claim-with-auto-approval (amount uint) (provider principal) (description (string-ascii 50)))
    (let
        (
            (policy (unwrap! (map-get? Policies tx-sender) ERR-POLICY-NOT-FOUND))
            (claim-id (+ (var-get total-claims) u1))
            (should-auto-approve (is-eligible-for-auto-approval tx-sender amount provider))
        )
        (asserts! (<= amount (get coverage-amount policy)) ERR-INVALID-AMOUNT)
        (asserts! (is-some (map-get? HealthcareProviders provider)) ERR-PROVIDER-NOT-FOUND)
        
        (map-set Claims claim-id
            {
                policy-holder: tx-sender,
                amount: amount,
                provider: provider,
                date: stacks-block-height,
                status: (if should-auto-approve "approved" "pending"),
                description: description
            }
        )
        (var-set total-claims claim-id)
        
        (if should-auto-approve
            (update-auto-approval-stats tx-sender amount)
            true
        )
        (ok claim-id)
    )
)

(define-private (is-eligible-for-auto-approval (holder principal) (amount uint) (provider principal))
    (let
        (
            (trusted-provider (map-get? TrustedProviders provider))
            (claim-history (default-to (list) (map-get? ClaimHistory holder)))
            (policy (unwrap! (map-get? Policies holder) false))
        )
        (and
            (var-get auto-approval-enabled)
            (<= amount (var-get auto-approval-amount-threshold))
            (is-eq (get status policy) "active")
            (>= (get end-date policy) stacks-block-height)
            (<= (len claim-history) (var-get auto-approval-claim-history-limit))
            (match trusted-provider
                provider-info (get auto-approval-eligible provider-info)
                false
            )
        )
    )
)

(define-private (update-auto-approval-stats (holder principal) (amount uint))
    (let
        (
            (current-stats (default-to 
                {total-auto-approved: u0, total-auto-approved-amount: u0, last-auto-approval: u0}
                (map-get? AutoApprovalStats holder)
            ))
        )
        (map-set AutoApprovalStats holder
            {
                total-auto-approved: (+ (get total-auto-approved current-stats) u1),
                total-auto-approved-amount: (+ (get total-auto-approved-amount current-stats) amount),
                last-auto-approval: stacks-block-height
            }
        )
    )
)

(define-public (configure-auto-approval (enabled bool) (amount-threshold uint) (claim-history-limit uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> amount-threshold u0) ERR-INVALID-THRESHOLD)
        (var-set auto-approval-enabled enabled)
        (var-set auto-approval-amount-threshold amount-threshold)
        (var-set auto-approval-claim-history-limit claim-history-limit)
        (ok true)
    )
)

(define-read-only (get-auto-approval-config)
    {
        enabled: (var-get auto-approval-enabled),
        amount-threshold: (var-get auto-approval-amount-threshold),
        claim-history-limit: (var-get auto-approval-claim-history-limit)
    }
)

(define-read-only (get-provider-trust-status (provider principal))
    (map-get? TrustedProviders provider)
)

(define-read-only (get-auto-approval-stats (holder principal))
    (map-get? AutoApprovalStats holder)
)

(define-read-only (check-auto-approval-eligibility (holder principal) (amount uint) (provider principal))
    (is-eligible-for-auto-approval holder amount provider)
)

(define-read-only (get-pending-claims-count)
    (let
        (
            (total (var-get total-claims))
        )
        (fold count-pending-claims (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0)
    )
)

(define-private (count-pending-claims (claim-id uint) (acc uint))
    (match (map-get? Claims claim-id)
        claim (if (is-eq (get status claim) "pending") (+ acc u1) acc)
        acc
    )
)

;; HSA Functions

;; Create a new HSA account
(define-public (create-hsa-account (is-catch-up-eligible bool))
    (let
        (
            (current-time stacks-block-height)
        )
        (asserts! (is-some (map-get? Policies tx-sender)) ERR-POLICY-NOT-FOUND)
        (asserts! (is-none (map-get? HSAAccounts tx-sender)) ERR-HSA-EXISTS)
        
        (map-set HSAAccounts tx-sender
            {
                balance: u0,
                annual-contributions: u0,
                contribution-year: current-time,
                last-interest-calculation: current-time,
                account-creation-date: current-time,
                is-catch-up-eligible: is-catch-up-eligible
            }
        )
        (var-set total-hsa-accounts (+ (var-get total-hsa-accounts) u1))
        (ok true)
    )
)

;; Contribute to HSA account
(define-public (contribute-to-hsa (amount uint))
    (let
        (
            (hsa-account (unwrap! (map-get? HSAAccounts tx-sender) ERR-HSA-NOT-FOUND))
            (current-time stacks-block-height)
            (current-contributions (get annual-contributions hsa-account))
            (base-limit (var-get hsa-annual-contribution-limit))
            (catch-up-limit (var-get hsa-catch-up-contribution-limit))
            (total-limit (if (get is-catch-up-eligible hsa-account) 
                (+ base-limit catch-up-limit) 
                base-limit))
            (transaction-id (+ (var-get total-hsa-transactions) u1))
        )
        (asserts! (> amount u0) ERR-INVALID-HSA-AMOUNT)
        (asserts! (<= (+ current-contributions amount) total-limit) ERR-CONTRIBUTION-LIMIT-EXCEEDED)
        
        ;; Reset annual contributions if new year
        (let
            (
                (updated-account (if (> (- current-time (get contribution-year hsa-account)) u52560)
                    (merge hsa-account {annual-contributions: u0, contribution-year: current-time})
                    hsa-account))
            )
            (map-set HSAAccounts tx-sender
                (merge updated-account
                    {
                        balance: (+ (get balance updated-account) amount),
                        annual-contributions: (+ (get annual-contributions updated-account) amount)
                    }
                )
            )
            
            ;; Record transaction
            (map-set HSATransactions transaction-id
                {
                    account-holder: tx-sender,
                    transaction-type: "contribution",
                    amount: amount,
                    date: current-time,
                    description: "HSA contribution",
                    claim-id: none
                }
            )
            (var-set total-hsa-transactions transaction-id)
            (ok transaction-id)
        )
    )
)

;; Withdraw from HSA for qualified medical expenses
(define-public (withdraw-from-hsa (amount uint) (description (string-ascii 100)) (claim-id (optional uint)))
    (let
        (
            (hsa-account (unwrap! (map-get? HSAAccounts tx-sender) ERR-HSA-NOT-FOUND))
            (current-time stacks-block-height)
            (transaction-id (+ (var-get total-hsa-transactions) u1))
        )
        (asserts! (> amount u0) ERR-INVALID-HSA-AMOUNT)
        (asserts! (>= (get balance hsa-account) amount) ERR-INSUFFICIENT-HSA-BALANCE)
        
        (map-set HSAAccounts tx-sender
            (merge hsa-account
                {
                    balance: (- (get balance hsa-account) amount)
                }
            )
        )
        
        ;; Record transaction
        (map-set HSATransactions transaction-id
            {
                account-holder: tx-sender,
                transaction-type: "withdrawal",
                amount: amount,
                date: current-time,
                description: description,
                claim-id: claim-id
            }
        )
        (var-set total-hsa-transactions transaction-id)
        (ok transaction-id)
    )
)

;; Calculate and apply interest to HSA account
(define-public (apply-hsa-interest (account-holder principal))
    (let
        (
            (hsa-account (unwrap! (map-get? HSAAccounts account-holder) ERR-HSA-NOT-FOUND))
            (current-time stacks-block-height)
            (time-since-last-calc (- current-time (get last-interest-calculation hsa-account)))
            (annual-rate (var-get hsa-annual-interest-rate))
            (interest-amount (/ (* (get balance hsa-account) annual-rate time-since-last-calc) (* u52560 u10000)))
            (transaction-id (+ (var-get total-hsa-transactions) u1))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> time-since-last-calc u0) ERR-INVALID-HSA-AMOUNT)
        
        (map-set HSAAccounts account-holder
            (merge hsa-account
                {
                    balance: (+ (get balance hsa-account) interest-amount),
                    last-interest-calculation: current-time
                }
            )
        )
        
        ;; Record interest transaction
        (map-set HSATransactions transaction-id
            {
                account-holder: account-holder,
                transaction-type: "interest",
                amount: interest-amount,
                date: current-time,
                description: "Interest payment",
                claim-id: none
            }
        )
        (var-set total-hsa-transactions transaction-id)
        (ok interest-amount)
    )
)

;; Pay claim using HSA funds
(define-public (pay-claim-with-hsa (claim-id uint))
    (let
        (
            (claim (unwrap! (map-get? Claims claim-id) ERR-CLAIM-NOT-FOUND))
            (hsa-account (unwrap! (map-get? HSAAccounts tx-sender) ERR-HSA-NOT-FOUND))
            (claim-amount (get amount claim))
        )
        (asserts! (is-eq tx-sender (get policy-holder claim)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status claim) "approved") ERR-NOT-AUTHORIZED)
        (asserts! (>= (get balance hsa-account) claim-amount) ERR-INSUFFICIENT-HSA-BALANCE)
        
        (try! (withdraw-from-hsa claim-amount "Claim payment" (some claim-id)))
        (ok true)
    )
)

;; HSA Read-only Functions

;; Get HSA account details
(define-read-only (get-hsa-account (account-holder principal))
    (map-get? HSAAccounts account-holder)
)

;; Get HSA transaction details
(define-read-only (get-hsa-transaction (transaction-id uint))
    (map-get? HSATransactions transaction-id)
)

;; Calculate remaining contribution limit for current year
(define-read-only (get-remaining-contribution-limit (account-holder principal))
    (match (map-get? HSAAccounts account-holder)
        hsa-account (let
            (
                (base-limit (var-get hsa-annual-contribution-limit))
                (catch-up-limit (var-get hsa-catch-up-contribution-limit))
                (total-limit (if (get is-catch-up-eligible hsa-account) 
                    (+ base-limit catch-up-limit) 
                    base-limit))
                (current-contributions (get annual-contributions hsa-account))
            )
            (some (- total-limit current-contributions))
        )
        none
    )
)

;; Check if HSA account exists
(define-read-only (has-hsa-account (account-holder principal))
    (is-some (map-get? HSAAccounts account-holder))
)

;; Get total HSA accounts
(define-read-only (get-total-hsa-accounts)
    (var-get total-hsa-accounts)
)

;; Get HSA configuration
(define-read-only (get-hsa-config)
    {
        annual-contribution-limit: (var-get hsa-annual-contribution-limit),
        catch-up-contribution-limit: (var-get hsa-catch-up-contribution-limit),
        annual-interest-rate: (var-get hsa-annual-interest-rate)
    }
)

;; HSA Administrative Functions

;; Update HSA contribution limits (only contract owner)
(define-public (update-hsa-contribution-limits (new-annual-limit uint) (new-catch-up-limit uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (> new-annual-limit u0) ERR-INVALID-HSA-AMOUNT)
        (asserts! (> new-catch-up-limit u0) ERR-INVALID-HSA-AMOUNT)
        (var-set hsa-annual-contribution-limit new-annual-limit)
        (var-set hsa-catch-up-contribution-limit new-catch-up-limit)
        (ok true)
    )
)

;; Update HSA interest rate (only contract owner)
(define-public (update-hsa-interest-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (<= new-rate u10000) ERR-INVALID-HSA-AMOUNT)
        (var-set hsa-annual-interest-rate new-rate)
        (ok true)
    )
)

