# LamLisp vs. Scheme R7RS - compatibility matrix

### Type predicates
boolean?
char? eof-object?
null? number?
pair? port?
procedure? string?
symbol? vector?

bytevector?

### Proper tail recursion

### Procedures
lambda define

### Conditionals
if cond case and or not

when unless

cond-expand (r5rs NO LL NO)


### Assignments and binding
set! define let let* letrec
let-values let*values define-values

### Evaluation and quotation
quote, quasiquote, unquote, unquote-splicing
Reader macros ' ` , ,@

do delay force delay-force promise? make-promise

### Dynamic bindings
make-parameter
parameterize

### Miscellaneous
case-lambda

### Macros
syntax-rules
let-syntax letrec-syntax define-syntax syntax-error
nlambda

### Libraries and Importing
import only except prefix rename define library

### Records
define-record-type

### Standard procedures
eq? eqv? equal?

### Numeric types and operations
Number types: 32-bit signed integer, 64-bit floating point
Number types: complex
Number types: rational

number? complex? real? rational? integer? exact? inexact? exact-integer? finite? infinite?
nan? zero? positive? negative? odd? even?
abs max min + - * /
Numeric comparisons < <= = >= >
floor ceiling truncate round floor/ floor-quotient floor-remainder truncate/ truncate-quotient truncate-remainder
numerator denominator
abs exp expt log square sqrt
sin cos tan asin acos atan
exact-integer-sqrt
make-rectangular make-polar real-part imag-part magnitude angle
number->string string->number

### Pairs and Lists
pair? cons car cdr set-car! set-cdr! null? list?
atom?
make-list list 
caar .. cddr (all combinations of car & cdr)
caaar .. cddddr 
append a reverse list-tail list-ref list-set! list-copy!
reverse!
memq memv member assq assv assoc

### Symbols
symbol? symbol=? symbol->string string->symbol

### Characters
char? char=? char<? char>? char<=? char>=?
Case-independent char-ci-* functions
char-upcase char-downcase
char-foldcase

### Strings
string? make-string string string-length string-ref string-set!
string<? string <=? string=? string>=? string>?
Case-independent string-ci functions
substring string-append string->list list->string string-copy string-copy! string-fill!

### Vectors
vector? make-vector vactor-length vector-ref vector-set! vector->list list->vector
vector->string
string->vector
vector->alist alist->vector
vector-copy vector-fill!

### Bytevectors
bytevector? make-bytevector bytevector bytevector-length bytevector-u8-ref bytevector-u8-set! bytevector-copy bytevector-append
utf8->string string->utf8

### Control Features
procedure? apply map string-map vector-map for-each 
string-for-each vector-for-each
call-with-current-continuation

values call-with-values
dynamic-wind

### Exception handling
guard raise
with-exception-handler
raise-continuable
error error-object? error-object-message error-object-irritants
read-error? file-error?

### Environments
environment
scheme-report-environment null-environment interaction-environment eval

### Ports
call-with-port call-with-input-file call-with-output-file
port? input-port? output-port? textual-port? binary-port?
input-port-open? output-port-open?
current-input-port current-output-port current-error-port
with-input-from-file with-output-to-file
open-input-file open-binary-input-file open-output-file open-output-binary-file
close-port close-input-port close-output-port
open-input-string
open-output-string get-output-string
open-input-bytevector
open-output-bytevector

### Input and Output
read read-char read-line eof-object? char-ready? read-string read-u8 peek-u8 u8-ready? read-bytevector read-bytevector!
write
write-shared
write-simple
display newline write-char write-string write-u8 write-bytevector flush-output-port

### System Interface
load
file-exists? delete-file
command-line
exit emergency-exit
get-environment-variables
current-second current-jiffy jiffies-per-second
features

