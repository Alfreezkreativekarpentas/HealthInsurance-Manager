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


