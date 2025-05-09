#ifndef LL_LAMB_H
#define LL_LAMB_H

#include "unistd.h"
#include "Arduino.h"

/*! \file
  This file is the header file required to use LambLisp.

  It is in 3 parts:
  -  Part 1 contains definitions for general-purpose low-level data types and utility functions.
  -  Part 2 defines the Cell structure and its accessors.
  -  Part 3 defines the LambLisp Virtual Machine.
*/

//!The longest and most accurate version of physical constants that we can make use of.
static const long double pi_long_double = 3.141592653589793238462643383279502884197;
static const long double eulers_number = 2.718281828459045235360287471352;

typedef byte uuid_t[16];

#define NOTUSED __attribute__((__unused__))			/*!<GCC extension to suppress individual "not used" warnings. */
#define INLINE __attribute__((__inline__))			/*!<GCC extension to encourage inlining a function. */
#define NOINLINE __attribute__((__noinline__))			/*!<GCC extension to avoid inlining a function. */
#define CHECKPRINTF __attribute__((format(printf, 1, 2)))	/*!<GCC extension, turn on checking of printf() const format strings, if they are known at compile time. */
#define CHECKPRINTF_pos2 __attribute__((format(printf, 2, 3)))	/*!<GCC extension, turn on checking of printf() const format strings, if they are known at compile time. */
#define CHECKPRINTF_pos3 __attribute__((format(printf, 3, 4)))	/*!<GCC extension, turn on checking of printf() const format strings, if they are known at compile time. */
#define CHECKPRINTF_pos4 __attribute__((format(printf, 4, 5)))	/*!<GCC extension, turn on checking of printf() const format strings, if they are known at compile time. */
#define ME(_me_) NOTUSED const char me[] = _me_			/*!<Declare an identifier "const char me[]" without causing "unused variable" warnings and/or code clutter to suppress them. */
#define isdef(sym) (#sym[0])					//!<Determine (cheaply) at runtime if a preprocessor symbol is defined.

//!Do something once after the system awakens.
#define once(_once_something) do {		\
    static bool _visited_ = false;		\
    if (!_visited_) {				\
      _visited_ = true;				\
      { _once_something; }			\
    }						\
  } while (0)					\
    //
//

//!Do something every so often.
#define every(_every_so_often_ms, _every_something_to_do) do {	\
    static unsigned long _every_next = 0;			\
    unsigned long _every_now = millis();			\
    if (_every_now >= _every_next) {				\
      { _every_something_to_do; }				\
      _every_next = _every_now + (_every_so_often_ms);		\
    }								\
  } while (0)							\
    //
//

//! @name LambLisp imposes a limit on the length of strings, to reduce opportunities for runaway in an embedded system.
//!@{
const unsigned long toString_MAX_LENGTH = 8192;		//!<Global limit on Lamb-generated strings.
void global_printf(const char *fmt, ...);		//!<This function puts a limit of 8k on generated strings.
String toString(const char *fmt, ...) CHECKPRINTF;	//!<Produce a new string from the format and arguments, respecting the global limit on string length.
//!@}

//! @name Useful global functions
//!@{
bool testbit(byte *b, unsigned long bit);		//!<Utility function to test a single bit in a large bit array.
bool setbit(byte *b, unsigned long bit, bool set);	//!<Utility function to set a single bit in a large bit array.

unsigned long roundup(unsigned long val, unsigned long nearest);	//!<Round up the value to the nearest multiple, and return the result.
unsigned long roundup2n(unsigned long val);				//!<Round up the value to the nearest power of 2, and return the result.

//!@}

/*!
  The embedded debug catcher ensures that an address is available to be set as a breaskpoint for a hardware debugger.
  This useful in cases where the generated code has been heavily inlined.
  Undefine this symbol if not using a hardware debugger, or redefine it to point to a different breakpoint target.
*/

void embedded_debug_catcher();
#define ll_debug_catcher //embedded_debug_catcher()

//! @name C++ *try* and *catch* are used to process code faults detected by the LambLisp VM.
//! To cleanly unwind after an error is detected, each *catch* has uniform bahvior, which is captured in this macro.
//!@{
#define ll_try try

#define ll_catch(__code_before_rethrow__)				\
  catch (Sexpr_t __err__) {						\
    if (__err__->type() != Cell::T_ERROR)				\
      throw NIL->mk_error("ll_catch() BUG in %s bad type %s", me, __err__->dump().c_str()); \
    global_printf("%s ll_catch(): %s\n", me, __err__->error_get_chars()); \
    ll_debug_catcher;							\
    __code_before_rethrow__;						\
    throw __err__;							\
  }									\
  //
//!@}

//! @name These primitive types are shared by LambLisp and the underlying VM.
//! There is a (mostly) obvious correspondence between the shared type name and the underlying C++ type.
//! The naming convention is:
//! \code typedef <underlying C++ type> <shared type> \endcode
//! This is done so that in future (e.g., for a 64-bit implementation) we could change the shared *Int_t* type to be
//! the C++ type *unsigned long* or Unix standard type *int64_t*, and the rest of the code will need no change.
//!
//! Note the difference between *Charst_t* and *CharVec_t*; one is mutable, the other not, to support strings located in read-only memory.
//! The same applies to *Bytest_t* and *ByteVec_t*.
//!@{
typedef unsigned char Byte_t;	//!<Universally known byte type, aka #u8 in Scheme R7RS.
typedef bool Bool_t;		//!<Boolean type.
typedef char Char_t;		//!<Character type.
typedef int Int_t;		//!<Integer type
typedef double Real_t;		//!<Real type

typedef unsigned Word_t;		//!<This is a generic computer word, used only for setting and retrieval.  It is not an *unsigned int* used for arithmetic.
typedef void *Ptr_t;			//!<Generic pointer in C++, 
typedef Char_t const *Charst_t;		//!<Pointer to immutable character array.
typedef Byte_t const *Bytest_t;		//!<Pointer to immutable byte array
typedef Char_t *CharVec_t;		//!<Pointer to mutable char array
typedef Byte_t *ByteVec_t;		//!<Pointer to mutable byte array
//!@}

//! @name Scheme partially defines *ports*, but LambLisp tries to keep them abstracted as far as possible, traceable back to the RxRS specifications.
//!@{
class LL_Port;			//!<This is a placeholder for the underlying C++ port defined elsewhere,  Here we only need pointers to instances of it.
typedef LL_Port Port_t;		//!<An alias for the underlying port implementation, conforming to the LambLisp VM shared type nomenclature.
typedef Port_t *Portst_t;	//!<A pointer to the shared type representing a *port* as described in the RxRS specifications.

//!@}

class Cell;		//forward
typedef Cell *Sexpr_t;	//a symbolic expression is a pointer to a cell

/*! @name These singletons exist outside the general cell population.
  Lisp implementations are notoriously variable on the type and details of NIL.  In some, NIL is a *well-known symbol*, whose type is *list* and
  whose *car* and *cdr* are also NIL (pointing to itself).

  In *Scheme*, there is no symbol NIL but the empty parenthesis are used as in ().  The empty list is its own type, which is is a list, and an atom, but not a pair.
  In LambLisp, the singleton Cell NIL is allocated statically with the appropriate initializers to fill the role.
  
  Several other statically allocated symbols are also defined in this group.  They are available at compile time.
*/
//!@{
extern Sexpr_t NIL;		//!<The empty list.
extern Sexpr_t HASHT;		//!<The canonical *true* value.
extern Sexpr_t HASHF;		//!<The canonical *false* value.
extern Sexpr_t OBJ_EOF;		//!<A single EOF object is used to indicate end-of-file on all ports.
extern Sexpr_t OBJ_UNDEF;	//!<A single UNDEF object is used to initialize a Cell having no immediate default value, or to indicate an unspecified value (in RxRS) that should not be encountered by the evaluator.
extern Sexpr_t OBJ_VOID;	//!<A silent version of OBJ_UNDEF.  It will not cause an error and will produce no output.  Use with care because it will hide potential error that would otherwise be obvious.
extern Sexpr_t OBJ_SYSERROR;	//!<A single instance of T_ERROR, statically alloocated with a fixed size buffer (also statically allocated ) for use by Cell-level errors
extern Char_t SYSERROR_protected_buffer[];	//!<A reserved message buffer used by the Cell-level OBJ_SYSERROR cell.  This buffer is statically allocated and is not garbage collected.
//!@}

/*!
  The Cell class is the foundational class for the LambLisp Virtual Machine.
  Nearly all the Cell methods are *inline* for performance.
  A Cell has only getters and setters; there are no other side effects.
  Cell fields are changed only through explicit requests, and not as the result of any other mutations.
  This means that Cells do not participate in garbage collection, which is managed from outside the Cell class.
  Indeed, there is no concept with Cells that there might be a plurality of them; only the behavior of a single Cell is defined.

  A Cell may be one of several types.  Historically, the number of types varied with the variant of Lisp.
  In LambLisp, there are Cell types that correspond directly to the types described in the *Scheme RxRS* specifications (integers, procedures etc,
  and there are additional types that implement underlying behavior to support the higher-level language (thunks, hash tables).

  For example, LambLisp supports several types of *strings* internally, depending on whether the character are stored on the heap (RAM),
  in read-only memory (ROM), or immediately within the cell (for fast operations on short strings).  Each of these *string* types
  is compatible with the string type in the *Scheme RxRS* specifications.

  The Cell type enumeration is ordered in such a way as to allow integer comparisons instead of a C switch in common cases.
  The types are grouped as follows:
  - Pairs vs. non-pairs (atoms).  Pairs require no special sub-marking or finalizing during garbage collection.
  - Of the atoms, a subset requires specialized finalizing.
  - Of those atoms requiring specialized finalizing, a subset requires specialized marking.

  The garbage collector states are ordered for similar reasons.  See the garbage collector chapter for details on the Cell life cycle.
*/
class Cell {
public:
  /*! @name Cell constructors - Avoid static Cell allocation if possible.
    In general, it is best not to do anything complex at static construction time.  There is no guarantee that dependencies will be ready.
    In particular, use of the terminal may cause the system to crash.  Leave any construction activities to more purposeful code at runtime.
    In the case of special cells (NIL etc) we require a static constructor.
  */
  ///@{
  Cell() {}
  Cell(Word_t typ, Word_t  w1, Word_t  w2)	{ set(typ, w1, w2); }
  Cell(Word_t typ, Int_t   w1, Sexpr_t w2)	{ set(typ, w1, w2); }
  Cell(Word_t typ, Sexpr_t w1, Sexpr_t w2)	{ set(typ, w1, w2); }
  ///@}
  
  enum {
    //Complex atoms first
    T_SVEC_HEAP=0,	//!<(Int_t Sexpr_t[])	A vector of Sexprs is a (len Sexpr_t*) pair.
    T_SVEC2N_HEAP,	//!<(Int_t Sexpr_t[]) 	Same as vector, but length must be a power of 2; useful for hash tables.
    
    T_SYMBOL,		//!<(Int_t T_STRING)	Symbol is a (hash T_STRING) pair.
    T_REQUIRES_SUBMARKING = T_SYMBOL,
    
    T_PORT,		//!<(reserved  Ptr_t)	car is unused, cdr is ptr to underlying C++ port instance
    T_BVEC_HEAP,	//!<(Int_t Bytest_t)	Bytevector is a (len byte*) pair.
    T_STR_HEAP,		//!<(Int_t Charst_t)	String is a (len char*) pair.
    T_REQUIRES_FINALIZING = T_STR_HEAP,
    
    T_BVEC_ROM,		//!<(Int_t Bytest_t)	Same as T_BYTEVEC but immutable; the byte array is not freed at GC time.
    T_STR_ROM,		//!<(Int_t Charst_t)	Same as T_STR but immutable; the character array is not freed at GC time.

    T_BVEC_IMM,		//!<Special format: type and flag bytes as usual, byte 2 is vector length, remaining bytes are vector elements.
    T_STR_IMM,		//!<Special format: type and flag bytes as usual, remaining bytes are 0-terminated string embedded in the cell)

    //Simple atoms next
    T_BOOL,		//!<(Bool_t reserved)	Boolean atom
    T_CHAR,		//!<(Char_t reserved)	Character atom
    T_INT,		//!<(Int_t reserved)	Integer atom
    T_REAL,		//!<(Special format: Real_t uses double word)	Real number atom
    T_RATIONAL,		//!<(Int_t Int_t)	The fields are numerator and denominator of a rational number.

    //Interface atoms to C++ functions.  No garbage collection finalization required.
    T_MOP3_PROC,	//!<(reserved *Mop3st_t)	pointer to native function - args are evaluated before calling
    T_MOP3_NPROC,	//!<(reserved *Mop3st_t)	pointer to native macro processor - args are not evaluated before calling

    //T_VOID is a singleton for functions that don't return a value.
    T_VOID,		//!<(don't care) VOID has its own type and a singleton instance OBJ_VOID

    //T_UNDEF type is a singleton with no substructure.
    T_UNDEF,		//!<(ERROR ERROR) UNDEF has its own type and a singleton instance OBJ_UNDEF

    /*!
      - The NIL type is both a list and an atom, but not a pair.
      - Types <= T_NIL are atoms, types >= T_NIL are lists, types > T_NIL are pairs.
    */
    T_NIL,		//!<(ERROR ERROR) NIL has its own type and a singleton instance.

    /*!
      - Pair types.  Types > NIL and types >= T_PAIR are pairs, but only T_PAIR responds to Scheme (pair?).
      - All pair types can be garbage collected without type-specific GC marking or finalizing.
    */
    T_PAIR,		//!<(Sexpr_t Sexpr_t) Normal untyped cons cell.  All C++ types < T_PAIR are atoms.
    T_SVEC1_IMM,	//!<(Sexpr_t Sexpr_t) Vector of 1 element.
    T_SVEC2_IMM,	//!<(Sexpr_t Sexpr_t) Vector of 2 elements.
    T_PROC,		//!<(Sexpr_t Sexpr_t) Procedure pair; car is lambda (formals + body containing free variables), cdr is environment.
    T_NPROC,		//!<(Sexpr_t Sexpr_t) Non-evaluating procedure pair; car is nlambda (formals + body containing free variables), cdr is environment.
    T_ENV,		//!<(Sexpr_t Sexpr_t) Environment pair; car is local frame, cdr is parent frames, special type helps to avoid circularity when printing.
    T_THUNK_SEXPR,	//!<(Sexpr_t Sexpr_t) S-expression thunk pair; car is sexpr, cdr is environment with all variable bindings.
    T_THUNK_BODY,	//!<(Sexpr_t Sexpr_t) Code body thunk pair; car is body (i.e., list of sexprs), cdr is environment with all variable bindings.
    T_ERROR,		//!<(NIL T_STR) An error cell; car is reserved at present, cdr is a string that will need finalization (except SYSERROR)

    Ntypes
  };

  //! @name Cell state flags, including gc states for multi-pass incremental gc, tail marker, and spares.
  ///@{
  static const int F_GC01 = 0x01;	//gc flags
  static const int F_GC02 = 0x02;	//
  static const int F_GC04 = 0x04;	//
  static const int F_TAIL = 0x08;	//trampoline tail marker
  static const int F_0x10 = 0x10;
  static const int F_0x20 = 0x20;
  static const int F_0x40 = 0x40;
  static const int F_0x80 = 0x80;
  static const int GC_STATE_MASK  = F_GC01 | F_GC02 | F_GC04;
  ///@}
  
#define _type_		(_contents._byte[0])
#define _flags_		(_contents._byte[1])
#define _car_		(_contents._word[1])
#define _cdr_		(_contents._word[2])
#define _car_ptr_	(&(_car_))
#define _realptr_	((Real_t *) _car_ptr_)
#define _byte2_ptr_	(&(_contents._byte[2]))

  //! @name Coarse, low-level cell field manipulation.
  //!@{
  void  zero()			{ _contents._word[0] = _contents._word[1] = _contents._word[2] = 0; }	//!<Set all cell bits to zero.
  Int_t type(void)		{ return _type_; }	//!<Return the type of the cell as a small integer.
  void  type(Int_t t)		{ _type_  = t; }	//!<Set the type of this cell.

  Int_t flags(void)		{ return _flags_; }	//!<Return the entire set of cell flags.
  void  flags_set(Int_t f)	{ _flags_ |= f; }	//!<Set the selected flags.
  void  flags_clr(Int_t f)	{ _flags_ &= ~f; }	//!<Clear the selected flags.

  void  rplaca(Word_t p)	{ _car_ = p; }		//!<Replace the cell car field.  Called rplaca for historical reasons, and to distinguish it from set-car!, which must respect the GC flags.
  void  rplacd(Word_t p)	{ _cdr_ = p; }		//!<Replace the cell cdr field.  Called rplacd for historical reasons, and to distinguish it from set-car!, which must respect the GC flags.
  //!@}

  //! @name Cell type property testing.
  //!@{
  Bool_t atom_requires_submarking(void)	{ return _type_ <= T_REQUIRES_SUBMARKING; }		//!<Return true if the atom requires specialized submarking.
  Bool_t atom_requires_finalizing(void)	{ return _type_ <= T_REQUIRES_FINALIZING; }		//!<Return true if the atom requires specialized finalizing.
  Bool_t is_atom(void)			{ return _type_ <= T_NIL; }				//!<Return true if the cell is an atom.
  Bool_t is_list(void)			{ return _type_ >= T_NIL; }				//!<Return true if the cell is alist.
  Bool_t is_pair(void)			{ return _type_ == T_PAIR; }				//!<Return true if the cell is a cons pair.
  Bool_t not_pair(void)			{ return _type_ != T_PAIR; }				//!<Return true if the cell is not a cons pair.
  Bool_t is_anypair(void)		{ return _type_ >  T_NIL; }				//!<Return true if the cell is any pair type.
  Bool_t not_anypair(void)		{ return _type_ <= T_NIL; }				//!<Return true if the cell is not any pair type.

  Bool_t is_any_svec_atom()		{ return (_type_ == T_SVEC_HEAP) || (_type_ == T_SVEC2N_HEAP) || (_type_ == T_SVEC2_IMM || T_SVEC1_IMM); }	//!<Return true if the cell is any kind of Sexpr_t vector.
  Bool_t not_any_svec_atom()		{ return !is_any_svec_atom(); }			//!<Return true if the cell is any kind of Sexpr_t vector.
  
  Bool_t is_any_str_atom(void)		{ return  (_type_ == T_STR_HEAP) || (_type_ == T_STR_ROM) || (_type_ == T_STR_IMM); }	//!<Return true if the cell is any kind of string.
  Bool_t not_any_str_atom(void)		{ return !is_any_str_atom(); }			//!<Return true if the cell is not any of the string types.

  Bool_t is_any_bvec_atom(void)		{ return (_type_ == T_BVEC_HEAP) || (_type_ == T_BVEC_ROM)|| (_type_ == T_BVEC_IMM); }	//!<Return true if the cell is a bytevector (mutable or immutable).
  Bool_t not_any_bvec_atom(void)	{ return !is_any_bvec_atom(); }				//!<Return true if the cell is a bytevector (mutable or immutable).

  Bool_t is_immediate_atom(void)	{ return (_type_ == T_SVEC2_IMM) ||(_type_ == T_BVEC_IMM) ||(_type_ == T_STR_IMM) || (_type_ == T_SVEC1_IMM); }
  Bool_t not_immediate_atom(void)	{ return !is_immediate_atom(); }
  
  //!@}

  //! @name Flag testing & setting for garbage and tail recursion functions.
  //!@{
  enum { gcst_idle, gcst_issued, gcst_stacked, gcst_marked, gcst_free, Ngcstates };
  
  Int_t   gc_state(void)	{ return _flags_ & GC_STATE_MASK; }							//!<Return the garbage collection stat eof this cell.
  Sexpr_t gc_state(Int_t st)	{ _flags_ =  (_flags_ & ~GC_STATE_MASK) | (st & GC_STATE_MASK);  return this; }		//!<Set the garbage collection state of this cell, and return the cell.
  Int_t   tail_state(void)	{ return _flags_ & F_TAIL; }								//!<Return the taill state of this cell.
  Sexpr_t tail_state_set(void)	{ _flags_ |=  F_TAIL;  return this; }							//!<Set the tail state flag on this cell, and return the cell.
  Sexpr_t tail_state_clr(void)	{ _flags_ &= ~F_TAIL;  return this; }							//!<Clear the tail state flag on this cell, and return the cell.
  //!@}
  
  Ptr_t  get_car_addr()		{ return _car_ptr_; }			//!<Return the address of the cell car.
  Word_t get_car(void)		{ return _car_; }			//!<Return the value of the cell car.
  Word_t get_cdr(void)		{ return _cdr_; }			//!<Return the value of the cell cdr.

  //! @name Field extractors for atom cells.
  //!@{
  Bool_t as_Bool_t()		{ return (Bool_t) _car_; }		//!<Return the value of this cell as a boolean.
  Char_t as_Char_t()		{ return (Char_t) _car_; }		//!<Return the value of this cell as a character.
  Int_t  as_Int_t()		{ return (Int_t)  _car_; }		//!<Return the value of this cell as an integer.
  Real_t as_Real_t()		{ return *_realptr_; }			//!<Return the value of this cell as a real number.

  Ptr_t     as_Ptr_t()		{ return (Ptr_t)     _cdr_; }		//!<Return the value of this cell as a generic pointer (i.e., void*).
  Charst_t  as_Charst_t()	{ return (Charst_t)  _cdr_; }		//!<Return the value of this cell as a pointer to a zero-terminated character array.
  Bytest_t  as_Bytest_t()	{ return (Bytest_t)  _cdr_; }		//!<Return the value of this cell as a pointer to an array of bytes.
  CharVec_t as_CharVec_t()	{ return (CharVec_t) _cdr_; }		//!<Return the value of this cell as a pointer to a zero-terminated character array.
  ByteVec_t as_ByteVec_t()	{ return (ByteVec_t) _cdr_; }		//!<Return the value of this cell as a pointer to an array of bytes.
  Portst_t  as_Portst_t()	{ return (Portst_t)  _cdr_; }		//!<Return the value of this cell as a pointer to an instance of the system underlying "port" implementation.
  
  CharVec_t as_str_heap()	{ return as_CharVec_t(); }		//!<Return a pointer to the character array in the heap.
  CharVec_t as_str_rom()	{ return as_CharVec_t(); }		//!<Return a pointer to the character array located outside the heap.
  CharVec_t as_str_imm()	{ return (CharVec_t) _byte2_ptr_; }	//!<Return a pointer to the character array embedded in this cell.
  
  Int_t as_numerator()		{ return (Int_t) _car_; }		//!<Return the value of this cell as the numerator of a rational number.
  Int_t as_denominator()	{ return (Int_t) _cdr_; }		//!<Return the value of this cell as the denominator of a rational number.
  //!@}
  
  Int_t hash(void);	//!<Return the hash value of this cell.

  //! @name Cell setters converting the C types into S-expressions.
  //!@{
  Sexpr_t set(Int_t typ, Word_t w1, Word_t w2)	{ _type_ = typ;  _car_ = w1;  _cdr_ = w2;  return this; }			//!<This is the lowest-level generic "set" function.
  Sexpr_t set(Int_t typ, Int_t a,   Sexpr_t b)	{ _type_ = typ;  _car_ = (Word_t) a;  _cdr_ = (Word_t) b;  return this; }	//!<This is a convenience function for common cases.
  Sexpr_t set(Int_t typ, Sexpr_t a, Sexpr_t b)	{ _type_ = typ;  _car_ = (Word_t) a;  _cdr_ = (Word_t) b;  return this; }	//!<This is a convenience function for common cases.
  
  Sexpr_t set(Real_t r)				{ _type_ = T_REAL;  *_realptr_ = r;  return this; }	//!<Set cell as real
  Sexpr_t set(Bool_t b)				{ return set(T_BOOL, (Word_t) b, (Word_t) NIL); }	//!<Set cell as boolean
  Sexpr_t set(Char_t c)				{ return set(T_CHAR, (Word_t) c, (Word_t) NIL); }	//!<Set cell as character
  Sexpr_t set(Int_t i)				{ return set(T_INT,  (Word_t) i, (Word_t) NIL); }	//!<Set cell as integer
  Sexpr_t set(Port_t &p)			{ return set(T_PORT, (Word_t) NIL, (Word_t) &p); }	//!<Set cell as port
  Sexpr_t set(Sexpr_t a, Sexpr_t b)		{ return set(T_PAIR, a, b); }				//!<Set cell as pair
  
  Sexpr_t set(Int_t typ, Int_t a, Charst_t b)	{ return set(typ, (Word_t) a, (Word_t) b); }	//!<Set cell as a immutable string
  Sexpr_t set(Int_t typ, Int_t a, Bytest_t b)	{ return set(typ, (Word_t) a, (Word_t) b); }	//!<Set cell as a immutable bytevector
  Sexpr_t set(Int_t typ, Int_t a, CharVec_t b)	{ return set(typ, (Word_t) a, (Word_t) b); }	//!<Set cell as a mutable string 
  Sexpr_t set(Int_t typ, Int_t a, ByteVec_t b)	{ return set(typ, (Word_t) a, (Word_t) b); }	//!<Set cell as a mutable bytevector
  //!@}
  
  Sexpr_t mk_error(const char *fmt, ...) CHECKPRINTF_pos2;	//!<This creates a cell of type T_ERROR.  At the level of Cell::mk_error(), there is only a single error cell which is always returned, but commonly with different contents.
  
  /*! @name Accessors for use when the type is known.
    If the cell type is already known, then these accessors can be used to efficiently access the cell contents.
  */
  ///@{
  Sexpr_t   prechecked_anypair_get_car()		{ return (Sexpr_t) get_car(); }
  Sexpr_t   prechecked_anypair_get_cdr()		{ return (Sexpr_t) get_cdr(); }

  Sexpr_t   prechecked_symbol_get_string()		{ return (Sexpr_t) as_Ptr_t(); }
  Int_t     prechecked_symbol_get_hash()		{ return as_Int_t(); }
  CharVec_t prechecked_symbol_get_chars()		{ return prechecked_symbol_get_string()->any_str_get_chars(); }

  Sexpr_t   prechecked_error_get_string()		{ return (Sexpr_t) as_Ptr_t(); }
  Charst_t  prechecked_error_get_chars()		{ Sexpr_t str = (Sexpr_t) get_cdr();  return (str == NIL) ? "" : str->any_str_get_chars(); }
  //! @}

  /*! @name Use these accessors if the cell type is unverified.
    These accessors will perform type checking and throw an error if an improper access is attempted.
  */
#define THROW_BAD_TYPE throw mk_error("%s Bad type %s", me, dump().c_str())
  ///@{
  Sexpr_t   anypair_get_car()		{ ME("Cell::anypair_get_car()");      if (is_anypair()) return prechecked_anypair_get_car(); THROW_BAD_TYPE; }
  Sexpr_t   anypair_get_cdr()		{ ME("Cell::anypair_get_cdr()");      if (is_anypair()) return prechecked_anypair_get_cdr(); THROW_BAD_TYPE; }
  Sexpr_t   symbol_get_string()		{ ME("Cell::symbol_get_string()");    if (_type_ == T_SYMBOL) return prechecked_symbol_get_string(); THROW_BAD_TYPE; }
  Charst_t  symbol_get_chars()		{ ME("Cell::symbol_get_chars()");     if (_type_ == T_SYMBOL) return prechecked_symbol_get_chars(); THROW_BAD_TYPE; }
  Int_t     symbol_get_hash()		{ ME("Cell::symbol_get_hash()");      if (_type_ == T_SYMBOL) return prechecked_symbol_get_hash(); THROW_BAD_TYPE; }
  Sexpr_t   error_get_string()		{ ME("Cell::error_get_string()");     if (_type_ == T_ERROR)  return prechecked_error_get_string(); THROW_BAD_TYPE; }
  Charst_t  error_get_chars()		{ ME("Cell::error_get_chars()");      if (_type_ == T_ERROR)  return prechecked_error_get_chars(); THROW_BAD_TYPE; }

  ///@}
  
  //!If the cell is any kind of string, return a pointer the zero-terminated character array.
  CharVec_t any_str_get_chars() {
    ME("Cell::anystring_get_chars()");
    if (_type_ == T_STR_HEAP) return as_str_heap();
    if (_type_ == T_STR_ROM) return as_str_rom();
    if (_type_ == T_STR_IMM) return as_str_imm();
    THROW_BAD_TYPE;
  }

  Int_t any_str_get_length() {
    ME("Cell::any_str_get_length()");
    Int_t Nchars = 0;
    if ((_type_ == T_STR_HEAP) || (_type_ == T_STR_ROM)) Nchars = as_Int_t();
    else if (_type_ == T_STR_IMM) {
      CharVec_t chars = as_str_imm();
      for (CharVec_t elems = chars; *elems; elems++)
	if (Nchars++ > (sizeof(Cell) - 2)) {
	  _contents._byte[sizeof(Cell) - 1] = 0;
	  throw mk_error("%s Unterminated %s\n", me, dump().c_str());
	}
    }
    else THROW_BAD_TYPE;

    return Nchars;
  }

  void any_str_get_info(Int_t &Nchars, CharVec_t &chars) {
    ME("Cell::any_str_get_info()");
    if (_type_ == T_STR_IMM) {
      Nchars = 0;
      chars  = as_str_imm();
      for (CharVec_t elems = chars; *elems; elems++)
	if (Nchars++ > (sizeof(Cell) - 2)) {
	  _contents._byte[sizeof(Cell) - 1] = 0;
	  throw mk_error("%s Unterminated %s\n", me, dump().c_str());
	}
    }
    else if (_type_ == T_STR_HEAP) {
      Nchars = as_Int_t();
      chars  = as_str_heap();
    }
    else if (_type_ == T_STR_ROM) {
      Nchars = as_Int_t();
      chars  = as_str_rom();
    }
    else THROW_BAD_TYPE;
  }


  void any_svec_get_info(Int_t &Nelems, Sexpr_t *&elems) {
    ME("Cell::any_svec_get_info()");
    if (_type_ == T_SVEC2_IMM) {
      Nelems = 2;
      elems  = (Sexpr_t *) _car_ptr_;
    }
    else if (_type_ <= T_SVEC2N_HEAP) {
      Nelems = as_Int_t();
      elems = (Sexpr_t *) as_Ptr_t();
    }
    else if (_type_ == T_SVEC1_IMM) {
      Nelems = 1;
      elems  = (Sexpr_t *) _car_ptr_;
    }
    else THROW_BAD_TYPE;
  }

  void any_svec2n_get_info(Int_t &Nelems, Sexpr_t *&elems) {
    ME("Cell::any_svec_get_info()");
    if (_type_ == T_SVEC2_IMM) {
      Nelems = 2;
      elems  = (Sexpr_t *) _car_ptr_;
    }
    else if (_type_ == T_SVEC2N_HEAP) {
      Nelems = as_Int_t();
      elems = (Sexpr_t *) as_Ptr_t();
    }
    else THROW_BAD_TYPE;
  }

  void prechecked_svec_heap_get_info(Int_t &Nelems, Sexpr_t *&elems) {
    Nelems = as_Int_t();
    elems = (Sexpr_t *) as_Ptr_t();
  }
  
  void any_bvec_get_info(Int_t &Nelems, ByteVec_t &elems)
  {
    ME("Cell::any_bvec_get_info()");
    if (_type_ == T_BVEC_IMM) {
      ByteVec_t b = (ByteVec_t) _byte2_ptr_;
      Nelems      = b[0];
      elems       = (ByteVec_t) &(b[1]);
    }
    else if ((_type_ == T_BVEC_HEAP) || (_type_ == T_BVEC_ROM)) {
      Nelems = as_Int_t();
      elems  = as_ByteVec_t();
    }
    else THROW_BAD_TYPE;
  }

#undef THROW_BAD_TYPE

    
  String   cell_name(void);		//!<A convenience feature to produce a string name for cells which are "well known" like NIL.  Otherwise the name is the hex representation of the cell address.

  Charst_t type_name(Int_t typ);	//!<Return a pointer to the C string corresponding to the cell type.
  Charst_t type_name(void) { return type_name(this->type()); }
  Charst_t gcstate_name(void);		//!<Return a pointer to a C string corresponding to the given GC state.

  String dump();			//!<Return a printable representation of the Cell internals.

#undef _type_
#undef _flags_
#undef _car_
#undef _cdr_
#undef _car_ptr_
#undef _realptr_
#undef _byte2_ptr_

  /*! @name Cell conversions to printable representation.
    These functions convert a Cell to a printable representation of the S-expression contents of the Cell.
    Because environments are often included in the descendants of the Cell being printed, the depth of environment recursiveness is limited.
  */
  //!@{
  String str(Sexpr_t sx, Bool_t as_write_or_display, Int_t env_depth, Int_t max_depth);
  String str(Bool_t as_write_or_display, Int_t env_depth, Int_t max_depth)	{ return str(this, as_write_or_display, env_depth, max_depth); }
  String str(Bool_t as_write_or_display, Int_t env_depth)			{ return str(this, as_write_or_display, env_depth, 10); }
  String str(Bool_t as_write_or_display)					{ return str(this, as_write_or_display, 1, 10); }
  String str(Int_t env_depth) 							{ return str(this, true, env_depth, 10); }
  String str(void)								{ return str(this, true, 1, 10); }
  //!@}
  
private:
  union {
    Byte_t _byte[3 * sizeof(Word_t)];
    Word_t _word[3];
  } _contents;

};

//! @name The LambLisp VM can accept S-expression generated externally, evaluate them, and produce the result as an S-expression.
//!@{
extern Sexpr_t LAMB_INPUT;	//!<If the variable is non-NIL, LambLisp will evaluate it and put the results in LAMB_OUTPUT;
extern Sexpr_t LAMB_OUTPUT;	//!<The result of evaluating LAMB_INPUT is placed here.
//!@}

class LambMemoryManager;
class LambPlatform;

/*!
  The Lamb class represents a single Lisp machine.  It is possible to create multiple indeopendent Lisp machines.
*/
class Lamb {
public:

  Lamb();
  
  //!This is a pointer to a native function that interacts directly with the top-level S-expression list.
  typedef Sexpr_t (*Mop3st_t)(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack);

  //! @name Arduino-compatible loop-based control interface.
  //!@{
  Sexpr_t setup(void);	//!<Run once after base platform has started.  In particular, `Serial` should be initialized before calling setup().
  Sexpr_t loop(void);	//!<Run often to maintain control.
  void end(void);	//!<Release resources used by the Lamb virtual machine.
  //!@}

  //! @name A few foundational functions for embedded debugging.
  //!@{
  void log(const char *fmt, ...) CHECKPRINTF_pos2;	//!<C-level printf feature with a limit on the length of strings produced.  Takes care of log prompt.
  void printf(const char *fmt, ...) CHECKPRINTF_pos2;	//!<C-level printf feature with a limit on the length of strings produced.
  bool debug(void);					//!<Return the state of the Lamb internal debug flag.
  void debug(bool onoff);				//!<Set the state of the Lamb debug flag.
  //!@}

  //! @name Interaction with the platform we are running on.
  //!@{
  const char *platform_name(void);		//!<Return a pointer to a chacter array containing a description of the runtime platform.
  void  platform_identification(void);		//!<Emit a string with the complete detailed description of the platform.

  void  platform_reboot(void);			//!<Do the obvious.
  unsigned long platform_free_heap();		//!<Return the unused space available for LambLisp expansion.  Whether this is *total* space or *largest* space is platform-dependent.
  unsigned long platform_free_stack();		//!<Return the unused stack space available for recursive expansion.  This is useful during debugging to detect runaway stack overflow before it causes a crash.
  void  platform_rand(byte *buf, Int_t len);	//!<Fill a buffer with the highest-quality random numbers available on this platform.
  unsigned long platform_loop_elapsed_us();	//!<Return the time elapsed since the start of this loop, in microseconds;
  unsigned long platform_loop_elapsed_ms();	//!<Return the time elapsed since the start of this loop, in milliseconds;
  
  //!@}
  
  //! @name Information about the current build.
  //!@{
  bool build_isDebug();			//!<Return true if this build is not checked in.  This is unrelated to the runtime `debug` flag.
  unsigned long build_version();	//!<Return the build version as a long integer (implemented as a UTC time stamp).
  unsigned long build_UTC();		//!<Return the UTC time of this build.
  unsigned long build_pushUTC();	//!<Return the UTC time this repo was last pushed.
  const char *build_buildRelease();	//!<Return a pointer to a character array containing the name of the runtime platform.
  const char *build_buildDate();	//!<Return a pointer to a character array containing the build date.
  const char *build_pushDate();		//!<Return a pointer to a character array containing the date this repo was last pushed.
  //@}

  /*! @name Cell constructors.
    Garbage collection may happen at any time during the executiono of these cell constructors.
    The cell constructors that accept S-expression arguments will protect those arguments from GC during the operation.
  */
  //!@{
  Sexpr_t tcons(Int_t typ, Word_t a,  Word_t  b, Sexpr_t env_stack);	//!<Generic cell constructor with no GC protection for its arguments.
  Sexpr_t tcons(Int_t typ, Int_t a,   Sexpr_t b, Sexpr_t env_stack);	//!<Constructor for (int, symbol) pair.  Will protect the S-expression argument from GC.
  Sexpr_t tcons(Int_t typ, Sexpr_t a, Sexpr_t b, Sexpr_t env_stack);	//!<Constructor for any pair type; protects both S-expression arguments.
  Sexpr_t cons(Sexpr_t a,  Sexpr_t b, Sexpr_t env_stack)		{ return tcons(Cell::T_PAIR, a, b, env_stack); }	//!<Cell constructor.
  //!@}
  
  //! @name A subset of the car/cdr accessors for list cells.  This is the subset used within LambLisp.
  //!@{
  Sexpr_t car(Sexpr_t l);
  Sexpr_t cdr(Sexpr_t l);
  Sexpr_t caar(Sexpr_t l)	{ return car(car(l)); }
  Sexpr_t cadr(Sexpr_t l)	{ return car(cdr(l)); }
  Sexpr_t cdar(Sexpr_t l)	{ return cdr(car(l)); }
  Sexpr_t cddr(Sexpr_t l)	{ return cdr(cdr(l)); }
  Sexpr_t caddr(Sexpr_t l)	{ return car(cddr(l)); }
  Sexpr_t cdddr(Sexpr_t l)	{ return cdr(cddr(l)); }
  Sexpr_t cadddr(Sexpr_t l)	{ return car(cdddr(l)); }
  Sexpr_t cddddr(Sexpr_t l)	{ return cdr(cdddr(l)); }
  //!@}
  
  //! @name The "bang" functions are the "mutators" of GC literature.  
  //!@{
  void set_car_bang(Sexpr_t c, Sexpr_t val);			//!<Replace the car field in the cell with *val*.  GC flags will be maintained as required.
  void set_cdr_bang(Sexpr_t c, Sexpr_t val);			//!<Replace the cdr field in the cell with *val*.  GC flags will be maintained as required.
  void vector_set_bang(Sexpr_t vec, Int_t k, Sexpr_t val);	//!<Replace the specified vector element with *val*.  GC flags will be maintained as required.
  Sexpr_t reverse_bang(Sexpr_t l);				//!<Reverse the list in-place and return the new list head (the former list tail).
  //!@}
  
  Sexpr_t quasiquote(Sexpr_t sexpr, Sexpr_t env_stack);

  //! @name Equivalence tests
  //!@{
  Sexpr_t eq_q(Sexpr_t obj1, Sexpr_t obj2);	//!<Return true if 2 cells are the same cell, or are atoms with the same value.
  Sexpr_t eqv_q(Sexpr_t obj1, Sexpr_t obj2);	//!<Return true if 1 cells are the same cell, or are atoms with the same value.
  Sexpr_t equal_q(Sexpr_t obj1, Sexpr_t obj2);	//!<Returns true when obj1 and obj2 are eqv?, and also all their descendants.
  //!@}
  
  Sexpr_t assq(Sexpr_t obj, Sexpr_t alist);

  Sexpr_t symbol_table_analyze(Sexpr_t sym_table, Int_t verbosity = 0);
  Sexpr_t symbol_test(const char *identifier)				{ return symbol_table_test(_lamb_symbol_table, identifier); }
  Sexpr_t symbol_intern_bang(const char *identifier, Sexpr_t env_stack)	{ return symbol_table_intern_bang(_lamb_symbol_table, identifier, env_stack); }
  Sexpr_t environment_analyze(Sexpr_t env, Int_t verbosity = 0);

  void  gc_root_push(Sexpr_t p);	//!<Preserve the cell given from GC, until popped.
  void  gc_root_pop(Int_t n=1);		//!<Release the preserved cells for normal GC processing.
  
  void  bind_bang(Sexpr_t env_target, Sexpr_t symbol, Sexpr_t value, Sexpr_t env_execution);	//Modify the target environment; `value` is (re-)assigned to `symbol` in the current frame.
  void  rebind_bang(Sexpr_t env_target, Sexpr_t symbol, Sexpr_t value, Sexpr_t env_execution);	//Modify the target environment; `value` is assigned to `symbol` wherever first found in env.

  Sexpr_t define_bang(Sexpr_t sexpr, Sexpr_t env_stack);					//Also a "bang" function, without the !bang.
  Sexpr_t variable_definition(Sexpr_t symbol, Sexpr_t env_stack);				//!<Returns (symbol value) pair, of #f if symbol is unbound
  Sexpr_t variable_value(Sexpr_t symbol, Sexpr_t env_stack);					//!<Returns the value associated with the symbol in the given environment; throws error if symbol is unbound.
  Sexpr_t add_frame(Sexpr_t formals, Sexpr_t vals, Sexpr_t env_target, Sexpr_t env_stack);	//!<Returns a new environment with top frame containing the formals bound to the values.

  Sexpr_t valq(Sexpr_t val, Sexpr_t env_stack);		//Search alist by matching cadrs instead of cars DEBT debug value only
  Sexpr_t value_get_variable(Sexpr_t val, Sexpr_t env_stack);
  Sexpr_t value_get_symbol(Sexpr_t val, Sexpr_t env_stack);
  
  Sexpr_t read(LL_Port &src, Sexpr_t env_stack)	{ return read_sexpr(src, env_stack); }

  Sexpr_t write_or_display(Sexpr_t sexpr, Bool_t do_write);
  Sexpr_t write_simple(Sexpr_t sexpr);

  String  sprintf(Charst_t fmt, Sexpr_t sexpr, Sexpr_t env_stack);

  Sexpr_t printf(Sexpr_t args, LL_Port &outp);
  Sexpr_t printf(Sexpr_t args);

  //! @name Evaluation and function application
  //! @{
  Sexpr_t eval_sexpr_partial(Sexpr_t sexpr, Sexpr_t env_stack);				//!<Evaluate the S-expression in the environment provided.  The result may be a tail requiring additional evaluation.
  Sexpr_t apply_proc_partial(Sexpr_t proc, Sexpr_t sexpr, Sexpr_t env_stack);		//!<Evaluate the function arguments and then apply the function to them.  The result may be a tail requiring further evaluation.
  Sexpr_t apply_proc_partial_noeval(Sexpr_t proc, Sexpr_t sexpr, Sexpr_t env_stack);	//!<Same as apply_proc, but does not evaluate its arguemnts.
  Sexpr_t map_proc(Sexpr_t proc, Sexpr_t lists, Sexpr_t env_stack);
  Sexpr_t resolve_mops(Sexpr_t unresolved, Sexpr_t env_stack);				//!<Replace symbols that evaluiate to mop3 native functions with the value of the symbol (i.i., the native function).
  //!@}

  //! @name Querying underlying system features.
  //!@{
  Sexpr_t r5_base_environment()		{ return _r5_base_environment; }
  Sexpr_t r5_interaction_environment()	{ return _r5_interaction_environment; }
  Sexpr_t current_symbol_table()	{ return _lamb_symbol_table; }
  
  Sexpr_t current_input_port()		{ return _r5_current_input_port; }
  Sexpr_t current_output_port()		{ return _r5_current_output_port; }
  Sexpr_t current_error_port()		{ return _r5_current_error_port; }
  //!@}
  
  Sexpr_t load(Charst_t name, Sexpr_t env_stack, Int_t verbosity = 0);
  
  //Some useful list processing used internally and available externally.
  Sexpr_t append(Sexpr_t sexpr, Sexpr_t env_stack);
  Sexpr_t list_copy(Sexpr_t sexpr, Sexpr_t env_stack);
  Sexpr_t list_analyze(Sexpr_t sexpr, Sexpr_t env_stack);
  Sexpr_t list_to_vector(Sexpr_t l, Sexpr_t env_stack);
  Sexpr_t vector_to_list(Sexpr_t v, Sexpr_t env_stack);
  Sexpr_t vector_copy(Sexpr_t from, Sexpr_t env_stack);

  Sexpr_t vector_to_sparsevec(Sexpr_t vec, Sexpr_t skip, Sexpr_t env_stack);
  Sexpr_t sparsevec_to_vector(Sexpr_t alist, Sexpr_t fill, Sexpr_t env_stack);

  Sexpr_t dict_ref(Sexpr_t obj, Sexpr_t dict);
  Sexpr_t dict_set_bang(Sexpr_t obj, Sexpr_t val, Sexpr_t dict, Sexpr_t env_stack);
  Sexpr_t dict_remove_bang(Sexpr_t obj, Sexpr_t dict, Sexpr_t env_stack);
  Sexpr_t dict_length(Sexpr_t dict, Sexpr_t env_stack);
  Sexpr_t dict_keys(Sexpr_t dict, Sexpr_t env_stack);
  Sexpr_t dict_values(Sexpr_t dict, Sexpr_t env_stack);
  
  //! @name The *makers* construct a new S-expression conforming to the parameters given.
  //!@{ 
  Sexpr_t mk_error(Sexpr_t env_stack, const char *fmt, ...) CHECKPRINTF_pos3;	//Return a new error object.
  Sexpr_t mk_syserror(const char *fmt, ...) CHECKPRINTF_pos2;	//Return a new error object.

  //Compact types
  Sexpr_t mk_bool(Bool_t b, Sexpr_t env_stack)		{ return cons(NIL, NIL, env_stack)->set(b);  }
  Sexpr_t mk_character(Char_t ch, Sexpr_t env_stack)	{ return cons(NIL, NIL, env_stack)->set(ch); }
  Sexpr_t mk_integer(Int_t i, Sexpr_t env_stack)	{ return cons(NIL, NIL, env_stack)->set(i);  }
  Sexpr_t mk_real(Real_t r, Sexpr_t env_stack)		{ return cons(NIL, NIL, env_stack)->set(r);  }
  Sexpr_t mk_sharp_const(Charst_t name, Sexpr_t env_stack);
  
  //Heap storage types
  Sexpr_t mk_number(Charst_t str, Sexpr_t env_stack);
  Sexpr_t mk_string(Sexpr_t env_stack, const char *fmt, ...) CHECKPRINTF_pos3;
  Sexpr_t mk_string(Int_t k, Charst_t src, Sexpr_t env_stack);
  Sexpr_t mk_symbol_or_number(Charst_t str, Sexpr_t env_stack);
  Sexpr_t mk_bytevector(Int_t k, Bytest_t src, Sexpr_t env_stack);
  Sexpr_t mk_bytevector(Int_t k, Int_t fill, Sexpr_t env_stack);
  Sexpr_t mk_bytevector(Int_t k, Sexpr_t env_stack);
  Sexpr_t mk_vector(Int_t len, Sexpr_t fill, Sexpr_t env_stack);
  Sexpr_t mk_hashtbl(Int_t len, Sexpr_t fill, Sexpr_t env_stack);
  Sexpr_t mk_symbol(Charst_t str, Sexpr_t env_stack)		{ return symbol_intern_bang(str, env_stack);  }

  Sexpr_t mk_serial_port(Sexpr_t env_stack);
  Sexpr_t mk_input_file_port(Charst_t name, Sexpr_t env_stack);
  Sexpr_t mk_output_file_port(Charst_t name, Sexpr_t env_stack);
  Sexpr_t mk_input_string_port(Charst_t inp, Sexpr_t env_stack);
  Sexpr_t mk_output_string_port(Sexpr_t env_stack);

  //Native procedures and syntax functions.
  Sexpr_t mk_Mop3_procst_t(Mop3st_t f, Sexpr_t env_stack)		{ return tcons(Cell::T_MOP3_PROC, (Word_t) NIL, (Word_t) f, env_stack);  }
  Sexpr_t mk_Mop3_nprocst_t(Mop3st_t f, Sexpr_t env_stack)		{ return tcons(Cell::T_MOP3_NPROC, (Word_t) NIL, (Word_t) f, env_stack); }

  //Pair types
  //mk_pair() is just cons().
  Sexpr_t mk_procedure(Sexpr_t formals, Sexpr_t body, Sexpr_t env_proc, Sexpr_t env_stack)	{ return tcons(Cell::T_PROC, cons(formals, body, env_stack), env_proc, env_stack); }
  Sexpr_t mk_nprocedure(Sexpr_t formals, Sexpr_t body, Sexpr_t env_nproc, Sexpr_t env_stack)	{ return tcons(Cell::T_NPROC, cons(formals, body, env_stack), env_nproc, env_stack); }
  Sexpr_t mk_environment(Sexpr_t frame, Sexpr_t base, Sexpr_t env_stack)	{ return tcons(Cell::T_ENV, frame, base, env_stack); }
  Sexpr_t mk_thunk_sexpr(Sexpr_t sexpr, Sexpr_t env_thunk, Sexpr_t env_stack)	{ return tcons(Cell::T_THUNK_SEXPR, sexpr, env_thunk, env_stack); }
  Sexpr_t mk_thunk_body(Sexpr_t body, Sexpr_t env_thunk, Sexpr_t env_stack)	{ return tcons(Cell::T_THUNK_BODY, body, env_thunk, env_stack); }
  //!@}
  
private:

  LambMemoryManager *mem;
  LambPlatform *platform;

  bool _debug_in_progress;
  Int_t _verbosity;
  
  //Symbols and bindings
  static const Int_t _symbol_table_size         = 2048;
  static const Int_t _r5_base_frame_size        = 1024;
  static const Int_t _r5_interaction_frame_size = 512;

  Sexpr_t _lamb_symbol_table;
  Sexpr_t _r5_base_environment;
  Sexpr_t _r5_interaction_environment;

  Sexpr_t _r5_current_input_port;
  Sexpr_t _r5_current_output_port;
  Sexpr_t _r5_current_error_port;
  
  //Reader
  const char *DELIMITERS = "()\";\f\t\v\n\r ";
  
  enum {
    TOK_EOF,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_DOT,
    TOK_ATOM,
    TOK_SQUOTE,
    TOK_DQUOTE,
    TOK_BQUOTE,
    TOK_COMMA,
    TOK_COMMA_AT,
    TOK_SHARP,
    TOK_SHARP_CONST,
    TOK_VECTOR,
    TOK_DCOLON,
    Ntokens
  };

  void report();
  
  Int_t produce_token(LL_Port &src);
  Sexpr_t consume_token(Int_t tok, LL_Port &src, Sexpr_t env_stack);

  //Tokens that the reader recognizes and converts directly to symbols.
  Sexpr_t sym_squote;		//single quote '
  Sexpr_t sym_qquote;		//backquote `
  Sexpr_t sym_unquote;		//comma ,
  Sexpr_t sym_uqsplice;		//comma-at ,@

  //Symbols inserted into the reader output to complete multi-step operations
  //e.g., a quoted vector '#(a b c) becomes (apply vector (quote (a b c)))
  Sexpr_t sym_apply;
  Sexpr_t sym_vector;

  //Magic reader tokens that turn into magic symbols.
  Sexpr_t sym_colon_hook;	//double colon ::
  Sexpr_t sym_sharp_hook;	//sharp # (not constant or vector)

  Sexpr_t sym_ellipsis;		//three dot ellipsis ...
  Sexpr_t sym_fatarrow;		//aka "feed through" =>
  
  //Loop-based control
  Sexpr_t sym_loop;
  
  Sexpr_t vector_eqv_q(Sexpr_t obj1, Sexpr_t obj2);
  Sexpr_t hashtbl_eqv_q(Sexpr_t obj1, Sexpr_t obj2);
  Sexpr_t bytevector_eqv_q(Sexpr_t obj1, Sexpr_t obj2);
  
  //Bindings: Symbols, Variables and Environments
  Sexpr_t member_symlist_identifier(Sexpr_t symbol_list, const char *identifier);
  Sexpr_t symbol_table_test(Sexpr_t symbol_table, const char *identifier);
  Sexpr_t symbol_table_intern_bang(Sexpr_t symbol_table, const char *identifier, Sexpr_t env_stack);

  Sexpr_t dump_frame(Sexpr_t frame);
  Sexpr_t dump_env_stack(Sexpr_t env_stack);

  //!The reader
  const char *token2name(Int_t tok);

  Sexpr_t read_sexpr(LL_Port &src, Sexpr_t &env_stack);
  Sexpr_t read_atom(LL_Port &src, Sexpr_t &env_stack);

  Sexpr_t read_any_quote(Sexpr_t symbol, LL_Port &src, Sexpr_t &env_stack);
  Sexpr_t read_squote(LL_Port &src, Sexpr_t env_stack)		{ return read_any_quote(sym_squote, src, env_stack); }
  Sexpr_t read_qquote(LL_Port &src, Sexpr_t env_stack)		{ return read_any_quote(sym_qquote, src, env_stack); }
  Sexpr_t read_unquote(LL_Port &src, Sexpr_t env_stack)		{ return read_any_quote(sym_unquote, src, env_stack); }
  Sexpr_t read_uqsplice(LL_Port &src, Sexpr_t env_stack)	{ return read_any_quote(sym_uqsplice, src, env_stack); }

  Sexpr_t read_sharp_const(LL_Port &src, Sexpr_t &env_stack);
  Sexpr_t read_sharp(LL_Port &src, Sexpr_t &env_stack);
  Sexpr_t read_vector(LL_Port &src, Sexpr_t &env_stack);
  Sexpr_t read_quotedvector(LL_Port &src, Sexpr_t &env_stack);  
  Sexpr_t read_list(LL_Port &src, Sexpr_t &env_stack);
  Sexpr_t read_string(LL_Port &src, Sexpr_t &env_stack);  

};

//With error check, inlined for speed.
inline Sexpr_t Lamb::car(Sexpr_t c)	{
  ME("Lamb::car()");
  if (c->type() >= Cell::T_PAIR) return c->prechecked_anypair_get_car();
  embedded_debug_catcher();
  throw mk_syserror("%s Bad type %s", me, c->dump().c_str());
}

inline Sexpr_t Lamb::cdr(Sexpr_t c)	{
  ME("Lamb::cdr()");
  if (c->type() >= Cell::T_PAIR) return c->prechecked_anypair_get_cdr();
  throw mk_syserror("%s Bad type %s", me, c->dump().c_str());
}

#include "ll_mop3.h"

#endif
