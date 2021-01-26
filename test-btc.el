(require 'ert)
(require 'ht)

(ert-deftest test-btc-get-url ()
  "Test `btc-get-url' function."
  (let ((btc-url "test/%s"))
    (should (equal (btc-get-url "usd") "test/usd"))))

(ert-deftest test-btc-json-get-rate ()
  "Tests for `btc-json-get-rate' function."
  (let ((obj (ht ("bpi" (ht ("USD" (ht ("code" "USD")
                                       ("rate" "32,065.7933")
                                       ("description" "United States Dollar")
                                       ("rate_float" 32065.7933))))))))
    (should (equal (btc-json-get-rate obj "usd") 32065.7933))))
