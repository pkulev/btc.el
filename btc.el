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

(require 'json)
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

(defcustom btc-format-string
  "%0.2f"
  "Specifier for `format' function to format rate value before displaying in modeline."
  :group 'btc
  :type 'string)

(defcustom btc-currency-code
  "usd"
  "Currency code to convert value to."
  :group 'btc
  :type 'string)

(defcustom btc-currency-sign
  "$"
  "Sign to show with BTC rate."
  :group 'btc
  :type 'string)

(defcustom btc-update-interval
  60
  "Update interval in seconds."
  :group 'btc
  :type 'integer)

(defun btc-format-rate (value)
  "Format rate VALUE for the mode-line."
  (concat " " btc-currency-sign (format btc-format-string value)))

(defvar btc-mode-line (btc-format-rate 0.000)
  "Rate to show in the mode-line.")

(defvar btc-timer nil
  "BTC update timer object.")

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
  (let* ((url (btc-get-url currency-code))
         (response-obj (request url :parser 'btc--json-parser :sync t))
         (data (request-response-data response-obj)))
    (btc-json-get-rate data currency-code)))

(defun btc-get-rate ()
  "Return BTC value using `btc-currency-code'."
  (btc-get-rate-by-code btc-currency-code))

(defun btc-update-mode-line% (value)
  "Update rate in the mode line with rate VALUE."
  (setq btc-mode-line (btc-format-rate value)))

(defun btc-update-mode-line ()
  "Update rate in the mode line."
  (btc-update-mode-line% (btc-get-rate)))

(defun btc-start-timer ()
  "Start timer."
  (unless btc-timer
    (setq btc-timer
          (run-with-timer 0
                          btc-update-interval
                          #'btc-update-mode-line))))

(defun btc-stop-timer ()
  "Stop timer."
  (when btc-timer
    (cancel-timer btc-timer)
    (setq btc-timer nil)
    (when (boundp 'mode-line-modes)
      (delete '(t btc-mode-line) mode-line-modes))))

(define-minor-mode btc-mode
  "Show BTC rate in the mode-line."
  :init-value nil
  :global t
  :lighter btc-mode-line
  (if btc-mode
      (btc-start-timer)
    (btc-stop-timer)))


(provide 'btc)

;;; btc.el ends here
