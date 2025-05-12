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

;; Data Variables
(define-data-var insurance-token-price uint u100)
(define-data-var total-policies uint u0)
(define-data-var total-claims uint u0)

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