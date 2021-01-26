;;; btc.el --- Get BTC value.

;; Copyright (C) 2021 Pavel Kulyov

;; Author: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Maintainer: Pavel Kulyov <kulyov.pavel@gmail.com>
;; Version: 0.1.0
;; Keywords: btc appearance
;; URL: https://www.github.com/pkulev/btc.git
;; Package-Requires: (request ht)

;; This file is NOT part of GNU/Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Get BTC value.

;;; Code:

(require 'request)

(defgroup btc nil
  "Get BTC value."
  :prefix "btc-"
  :group 'tools)

(defcustom btc-url
  "https://api.coindesk.com/v1/bpi/currentprice/%s.json"
  "Data source to get BTC value from."
  :group 'btc
  :type 'string)

(defcustom btc-currency-code
  "usd"
  "Currency code to convert value to."
  :group 'btc
  :type 'string)

(defun btc-get-url (currency-code)
  "Return URL for given CURRENCY-CODE."
  (format btc-url currency-code))

(defun btc--json-parser ()
  "Parse JSON data as hash table."
  (let ((json-object-type 'hash-table))
    (json-read)))

(defun btc-json-get-rate (obj currency-code)
  "Get BTC rate from JSON object OBJ by CURRENCY-CODE."
  (thread-last obj
    (gethash "bpi")
    (gethash (upcase currency-code))
    (gethash "rate_float")))

(defun btc-get-rate-by-code (currency-code)
  "Return BTC value by CURRENCY-CODE."
  (let* ((url (get-url currency-code))
         (response-obj (request url :parser 'btc--json-parser :sync t))
         (data (request-response-data response-obj)))
    (json-get-rate data currency-code)))

(defun btc-get-rate ()
  "Return BTC value using `btc-currency-code'."
  (btc-get-rate-by-code btc-currency-code))

(provide 'btc)

;;; btc.el ends here
