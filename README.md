placcounting
============

Simple double-entry bookkeeping console application written in Prolog. It is currently
compatible and tested with the [Swi-Prolog](http://www.swi-prolog.org/). The program
helps to prepare montly and annual reports and serves as a financial journal. It does
not support asset or inventory management, client handling or invoicing. Only single
currency is supported. For multiple currencies, conversion has to be done externally,
and the profit or loss from the conversion can then be logged into the separate account.

Installation
------------

The application requires Swi-Prolog and [Prolog-yamltiny](https://github.com/rla/prolog-yamltiny).
To install placcounting, run `make install`. The installation location depends on the install
prefix of the Swi-Prolog and for the system-wide install it requires the root privileges.

Usage
-----

Run `placcounting <directory>` where the directory contains suitable YAML files. Files
accounts.yml, reports.yml and settings.yml are considered to be special, all other
files ending in *.yml are assumed to contain financial transactions. The file accounts.yml contains
the list of accounts in the following format:

    code: 1
    name: Cash
    type: asset
    ---
    code: 2
    name: Bank
    type: asset
    ---
    code: 3
    name: Sales
    type: income
    ---
    code: 4
    name: Expenses
    type: expense
    ---
    code: 5
    name: Accounts payable
    type: liability
    ---
    code: 6
    name: Owner's equity
    type: equity

The code field assigns an unique account identifier to each account. It can be arbitrary (does
not have to be a number, could be some kind of a semantic tag). The account type can be one of
these: asset, liability, income, expense, equity. The type currently sets on which side of
the [Accounting equation](http://en.wikipedia.org/wiki/Accounting_equation) the account resides.
This is needed to calculate account balances.

Files reports.yml and settings.yml are reserved but not used in the current version. All other *.yml
files in the directory are assumed to contain transactions in the following format:

    date: 2012-01-01
    desc: Opening balances
    lines:
      - debit: 2
        credit: 6
        sum: 2556.00
    ---
    date: 2012-02-04
    desc: VPS hosting invoice #123, January
    lines:
      - debit: 4
        credit: 5
        sum: 14.0
    ---
    date: 2012-02-05
    desc: Paying VPS hosting invoice #123
    lines:
      - debit: 5
        credit: 2
        sum: 14.0
      - debit: 4
        credit: 2
        sum: 0.38
        desc: Bank's fee

Dates are in the Date-Only ISO8601 format. Under lines, multiple items might be
present. An item can have its own description. No special constructs are used for
opening or closing accounts, all of that can be and should be done with normal
transactions.

This example is also included in the example folder. Running `placcounting /path/to/example`
will bring up the Prolog console with the following output:

    cleaning database
    reading example/accounts.yml
    asserting 6 accounts
    reading example/all.yml
    asserting 3 transactions
    total accounts: 6
    total transactions: 3
    total lines: 4
    accounting system started.

Running a query `report('2')` for the bank account will bring up the following output:

    ?- report('2').
    2012-01-01 2 6 2556.00 Opening balances
    2012-02-05 4 2 0.38 Paying VPS hosting invoice (Bank's fee)
    2012-02-05 5 2 14.00 Paying VPS hosting invoice
    Debit: 2556.00
    Credit: 14.38
    Balance: 2541.62

Quoted account code '2' has to be used since account codes are not numbers
but just Prolog atoms.

Queries
-------

For more detailed info, please see thr documentation for the exported predicates in
source files src/analysis.pl and src/reporting.pl.

`read_all(Directory)`

Loads all *.yml files in the directory. Previously loaded
data is cleared from the memory. Running the placcounting command will
execute this automatically.

`report(Account)`

Outputs transaction lines for the given account. Reports total credit, total debit and balance.

`balance(Account, Balance)`

Calculates the balance for the given account.

`balance(Account, Month, Balance)`

Calculates the balance for the given account for the given month. This only
uses transactions done at the given month. It does not accumulate the
previous months. Month is specified as an integer. January is 1, February is 2 and so on.

`reread`

Re-reads currently loaded database files.

Debugging
---------

When you get the following error:

    ERROR: Unknown message: account_does_not_exist(6)

Then you have a transaction that uses an account that is not specified in
the accounts.yml file.

TODO
----

* Better and more reporting options.
* Report splitting by debit or credit source - cash flow reports need this.
* Reports should also allow account types not only specific accounts.
* Document how to use it as a Prolog library.
* Unit tests.

License
-------

    Copyright (c) 2013 Raivo Laanemets

    Permission is hereby granted, free of charge, to any person
    obtaining a copy of this software and associated documentation
    files (the "Software"), to deal in the Software without restriction,
    including without limitation the rights to use, copy, modify, merge,
    publish, distribute, sublicense, and/or sell copies of the Software,
    and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
    THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
    FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
    IN THE SOFTWARE.