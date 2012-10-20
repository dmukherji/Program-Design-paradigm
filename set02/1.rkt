;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "2htdp")))))
(require rackunit)
(require rackunit/text-ui)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide price-of-order)
(provide inventory-increase-prices)
(provide make-item item-sku item-name item-manufacturer item-unit-price 
         item-in-stock)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS
(define-struct item (sku name manufacturer unit-price in-stock))
;; An Item is a (make-item Number String String Number Number)
;; Interpretation:
;; sku is the Stock Number of this item
;; name is the name of this item
;; manufacturer is a String represent the manufacture of the item
;; unit-price is the price per unit of this item
;; in-stock is the number of this item in stock.
;; Invariant: in-stock is >= 0.
;; template:
;; item-fn : Item -> ??
;(define (item-fn i)
;   (...
;    (item-sku i)
;    (item-name i)
;    (item-manufacturer i)
;    (item-unit-price i)
;    (item-in-stock i)))

;; An Inventory is a ListOf<Item>.
;; --empty
;; --(cons Item Inventory)
;; Interp:
;; An Inventory has nothing in it;
;; or, a sequence with Item as the first element and another 
;; Invariant: In any inventory, there is at most one item with a given sku. 

;; template:
;; inventory-fn : Inventory -> ?
;(define (inventory-fn inventory)
;   (cond 
;       [(empty? inventory) ...]
;       [else (... (first inventory)
;                  (inventory-fn (rest inventory)))]))

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANT DEFINITIONS
(define EMPTY-INVENTORY empty)
(define iphone5 (make-item 101 "iphone5" "apple" 200 100))
(define iphone4s (make-item 102 "iphone4s" "apple" 100 200))
(define iphone4 (make-item 103 "iphone4" "apple" 50 300))
(define galaxy3 (make-item 104 "galaxy3" "samsung" 100 110))
(define lumia900 (make-item 105 "lumia900" "nokia" 200 5))

(define iphone5-price-increase-10% (make-item 101 "iphone5" "apple" 220 100))
(define iphone4s-price-increase-10%  (make-item 102 "iphone4s" "apple" 110 200))
(define iphone4-price-increase-10% (make-item 103 "iphone4" "apple" 55 300))
;;(define 2galaxy3 (make-item 104 "galaxy3" "samsung" 100 110))
;;(define 2lumia900 (make-item 105 "lumia900" "nokia" 200 5))

(define PHONE-INVENTORY (list iphone5 iphone4s iphone4 galaxy3 lumia900))
(define PHONE-INVENTORY-APPLE-PRICE-CHANGED (list iphone5-price-increase-10% 
                                                  iphone4s-price-increase-10% 
                                                  iphone4-price-increase-10% 
                                                  galaxy3 lumia900))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAIN FUNCTIONS

;; price-of-order : Inventory Number Number -> Number
;; it produces a total price for a given sku (second parameter)
;; quantity (3rd parameter) and a list of items (Inventory)
;; EXAMPLES:
;; (price-of-order PHONE-INVENTORY 102 4)=396
;; (price-of-order PHONE-INVENTORY 101 4)=796
;; strategy: structural decomposition on inventory [inventory] 
(define (price-of-order inventory sku quantity)
   (cond 
       [(empty? inventory) "Product doesn't exist"]
       [else (if (= (item-sku (first inventory)) sku)
                    (get-price (first inventory) quantity)
                    (price-of-order (rest inventory) sku quantity))]))

;; get-price : Item Number -> Number
;; return the total price for a certain number of the given product
;; example:
;; (get-price iphone5 10) = 2000
;; strategy: Domian Knowledge
(define (get-price item quantity)
    (if (>= quantity 0)
        (* quantity (item-unit-price item))
        "please enter a right quantity"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inventory-increase-prices : Inventory String Number -> Inventory
;; all items,which are made by the given manufacturer (String) in the Inventory
;; will increase their price by a pecentage (Number)
;; EXAMPLES:
;; (inventory-increase-prices phone_inventory "apple" 0.1)
;; =phone_inventory_price_changed
;; strategy: structural decomposition on inventory [inventory] 
(define (inventory-increase-prices inventory manufacturer percentage)
   (cond 
       [(empty? inventory) inventory]
       [else (cons (item-increase-price (first inventory) 
                                        manufacturer 
                                        percentage)
                   (inventory-increase-prices (rest inventory) 
                                              manufacturer 
                                              percentage))]))

;; item-increase-price : Item String Number -> Item
;; decide whether an item is made by manufacturer (String)
;; if so, increase unit price by percentage (Number)
;; examples:
;; (item-increase-price iphone5 "apple" 0.1) = 2iphone5
;; (item-increase-price lumia900 "apple" 0.1) = lumia900
;; strategy: function composition
(define (item-increase-price i m p)
   (if (string=? (item-manufacturer i) m)
       (make-item (item-sku i)
                  (item-name i)
                  (item-manufacturer i)
                  (* (item-unit-price i) (+ 1 p))
                  (item-in-stock i))
       i))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests for price-of-order
;; inventory : empty/non-empty
;; sku : exist/not exist
;; quantity : >= 0 /< 0
(define-test-suite price-of-order-tests
   (check-equal?
      (price-of-order PHONE-INVENTORY 101 5)
      1000
      "inventory : not empty; sku : exist; quantity : right")
   (check-equal?
      (price-of-order PHONE-INVENTORY 95 5)
      "Product doesn't exist"
      "inventory : not empty; sku : not exist; quantity : right")
  (check-equal?
      (price-of-order PHONE-INVENTORY 101 -1)
      "please enter a right quantity"
      "inventory : not empty; sku :exist; quantity : wrong")
   (check-equal?
      (price-of-order EMPTY-INVENTORY 101 1)
      "Product doesn't exist"
      "inventory : empty"))

;; Test design:
;; inventory : empty/non-empty
;; manufacturer : exist/not exist
;; total four tests
(define-test-suite increase-price-tests
   (check-equal?
      (inventory-increase-prices PHONE-INVENTORY "apple" 0.1)
      PHONE-INVENTORY-APPLE-PRICE-CHANGED
      "inventory : not empty; manufacturer : exist")
   (check-equal?
      (inventory-increase-prices PHONE-INVENTORY "ZTE" 0.1)
      PHONE-INVENTORY
      "inventory : not empty; manufacturer : not exist")
   (check-equal?
      (inventory-increase-prices EMPTY-INVENTORY "ZTE" 0.1)
      EMPTY-INVENTORY
      "inventory : empty; manufacturer : not exist"))
(run-tests price-of-order-tests)
(run-tests increase-price-tests)
