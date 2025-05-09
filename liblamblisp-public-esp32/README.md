# LambLisp - Real-time Lisp for embedded systems.
<div>
<img src=lamb-DALL-01.webp align="left" style="wrap" width="25%" />
<br>
</div>
<div style="wrap">
### LambLisp is Copyright 2025 Frobenius Norm LLC
<br>
***LambLisp*** is a real-time implementation of the ***Scheme*** dialect of the ***LISP*** programming language.
The *Scheme* dialect of Lisp is governed by standards known as *Scheme R5RS* and *Scheme R7RS*.
<br><br>
The main repository of Scheme standards and history can be found at *scheme.org*.
LambLisp is designed primarily to *Scheme R5RS*, with additional features from *Scheme R7RS* that aid in
development of real-time control applications.
<br><br>
*LambLisp* is now running on *Espressif ESP32* using the tools from *platformio*.
*LambLisp* has a built-in Arduino-style hardware abstraction layer, a standard way to introduce new devices using available C++ hardware drivers,
and many other features focused on high-performance embedded control applications.
</div>

# Acknowledgements
<div style="wrap">

I reviewed many Lisp and Scheme implementations, but the one I studied in detail was *TinyScheme* 1.42, writen in 5000+ lines of high-quality circa-1992 C code.
It was too big for the processor I was working on at the time (in 2020).
Ultimately I returned to the embedded Lisp problem, but made different design choices than the TinyScheme authors,
especially on the compile-vs-interpet spectrum and garbage collection techniques.  The study was hugely informative nonetheless.
<br><br>
*Chez Scheme* is the ultimate large implementation.
It is the work of a lifetime, primarily of one man, and a standard for performance and completeness in the Scheme world.
I used Chez to provide benchmark behavior in cases where the spec is not clear or offers a choice of outcomes.
<br><br>
I have included an extensive bibliography that also informed my efforts.  If the reader would like to better understand Lisp design, start there.
<br><br>
Bill White 2025
</div>

# LambLisp Key Features (not all in Alpha versions)
<div style="wrap">
| Key Features                                                                 |
|------------------------------------------------------------------------------|
| Designed to Scheme R5RS and R7RS specifications.                             |
| Optimized for microprocessors.  ESP32S3 is the current development platform. |
| Interpreter-oriented architecture for compact high performance.              |
| Arduino-compatible interfaces for digital & analog I/O, I2C, SPI, WiFi, etc. |
| Easy addition of manufacturer-provided C/C++ hardware drivers.               |
| Easy integration of application-specific C++ functions.                      |
| Leverages existing gcc-based tool chain, C runtime.                          |
| Adaptive incremental garbage collector for fast, uniform loop times.         |
| Selectable run-time packages (math, ethernet etc) to control program size.   |
| Virtual memory capability from remote or local storage                       |
| Incremental updates over-the-air without pausing control or rebooting.       |

</div>

## What's in the repository?
<div style="wrap">
LambLisp is delivered as a set of library library components that allow interoperability with embedded applications written in C++.
When LambLisp is started, it will load a file called setup.scm from the local file system.  This should be a Scheme source code file.
At a minimum, this file file needs to contain a definition for the function *loop*.

The C++ loop() function, which gets control periodically from the operating system, should look up the symbol *loop*, and request that LambLisp run it as a function.
This arrangement allows for scaling of the application between C++ and *Scheme*.

The root of the repository contains these files and directories:
| File or Dir        | Description                                                             |
|--------------------|-------------------------------------------------------------------------|
| platformio.ini     | sample configuration                                                    |
| doc                | contains the README and html documentation for LambLisp                 |
| src                | contains the sample main.cpp                                            |
| data               | contains setup.scm                                                      |
| lib                | contains:                                                               |
|                    | - libLambLisp.sa - LambLisp Virtual Machine shared archive              |
|                    | - source code - selected examples for extensibility                     |
| LambLisp-xx.bin    | Pre-built application binary (no bootloader, file system or partitions) |
| LambLisp-xx.tar.gz | Pre-built binary with bootloader, partitions, file system               |


</div>

# Lisp basics

<div style="wrap">
**LISP** is the original language of artificial intelligence, and is the second-oldest programming language still in use (after Fortran).
The *Scheme* variant has many features familiar to programmers in other languages, such as *lexical scoping* and *duck typing*,
as well as advanced features such as tail recursion, first-class procedures, and continuation-passing style.
<br><br>
Lisp is based on a theoretical foundation called *lambda calculus*, in which the objects of interest are functions to be applied to a (possibly empty) set of arguments.
The fundamental irreducible lambda expressions (constant, identity, equality, and a few others) are the axioms of the calculus, and their combination is the subject of interest.
<br><br>
In typical algebra notation, to compose two functions *f* and *g* that each require a single parameter *x* we write:

```
y = f(g(x))
```
The functions are composed using *prefix* notation. Algebra and C/C++ use a mix of parenthesis, prefix, infix, and postfix notation like this:
```
--x + abs(x) + 2 + (x++ - y[z])
```
Lambda calculus uses prefix notation but is not conducive to keyboard use:
```
λf. λg. λx.
```
Lisp uses prefix notation, with parenthesis and symbols being the only syntactical elements.  To compose functions *f* and *g*, we write:
```
(f (g x))
```
and to compare 2 elements *x* and *y*, there is the eq? operator:
```
(eq? x y)
```
The foundational Lisp language consists of data items called *cells* and a few predefined operations on them.
Cells may be either *symbols* (also referred to as *atoms*) or *pairs*.
The native functions that operate on cells appear in the language as "well-known" symbols such as *if* and *lambda*.
<br><br>
Cells are represented by 3-part structures.
The first part, called the *tag*, indicates whether the cell is a symbol or pair.
If a pair, the remaining two parts are referred to as *car* and *cdr* for historical reasons, and each contains a reference to another cell.
If a symbol, then the remaining parts are used to encode the details of the symbol.

<br><br>
The foundational Lisp features are:
<br>
| Feature               | Description                                                                                      |
|-----------------------|--------------------------------------------------------------------------------------------------|
| S-expressions         | Either a symbol or a list.                                                                       |
| symbol                | A series of non-space characters, also not containing parenthesis.                               |
| list                  | A set of parenthesis containing zero or more S-expressions separated by spaces.                  |
| procedures            | Defined using the *lambda* keyword                                                               |
| evaluation            | The Lisp machine reads S-expressions, evaluates them, and returns the result                     |
| evaluation of symbols | Symbols evaluate to themselves                                                                   |
| evaluation of lists   | The first item in the list must be a procedure and applied to the remaing arguments in the list. |

<br>

Upon this set of features are several primitive operations, from which all other operations can be derived;
<br>
| Operation                              | Description                                                                              |
|----------------------------------------|------------------------------------------------------------------------------------------|
| ARG                                    | Evaluate the argument (which may be an atom or list), and return the result              |
| (quote *ARG*)                          | Returns its argument unevaluated, may be abbreviated with single quote.                  |
| (eq? *ARG1* *ARG2*)                    | Return true if two S-expressions are the same S-expression.                              |
| (cons *ARG1* *ARG2*)                   | Construct a new cell, setting its *car* and *cdr* fields to ARG1 and ARG2, respectively. |
| (car *ARG*)                            | Return the value of the *car* field of ARG, which must be a list                         |
| (cdr *ARG*)                            | Return the value of the *cdr* field of ARG, which must be a list.                        |
| (if *test* *consequent* *alternative*) | Evaluate a test expression and then evaluate one or other option.                        |
| (lambda *ARGS* *BODY*)                 | Create a procedure accepting ARGS and executing BODY each time is it calld.              |
| (*function* *ARGS*)                    | Apply function F to arguments ARGS                                                       |

To create a function that can add 2 bits using these basics, we can do the following:

```lisp
(lambda (x y)
    (if (eq? x '1)
        (if (eq? y '1) '10 '01)
		(if (eq? y '1) 'O1 '00)
		)
	)
```

\note In the example above, the digits are not used as numbers, they are just symbols.  Symbols have no intrinsic meaning.  
The nature of symbols is how humans interpret them, and then how humans program machines to interpret them.  
The following code is equally valid, using different symbols.

```lisp
(lambda (x y)
    (if (eq? x 'ONE)
	    (if (eq? y 'ONE) 'TWO 'ONE)
		(if (eq? y 'ONE) 'ONE 'ZERO)
		)
	)
```

Operations of similar simplicity could be used to implement addition of integers of any size,
and that analogous functions for subtraction, multiplication, etc., can be constructed.
On this simple mathematic foundation, many Lisp implementations have been built.
<br><br>
Note that the operations above, being purely mathematical constructs, are not *programs*, but *axioms* and *reasoning* to produce the *result* of *evaluating* a *symbolic expression*.
There is, for example, no assignment operation in the small Lisp described above, which most applications would find limiting.
While assignment and many other useful functions have been added to make Lisp a practical language, they can all be derived from a small, core set of axioms and reasoning.
</div>

# Practical Lisp
<div style="wrap">
From the theoretical base beginning about 1960, Lisp quickly grew into a practical language with several well-supported variants.  The notion of *atoms* was extended beyond symbols.
Integers, real numbers, and strings were added as additional atom types, along with functions specialized for these atoms (addition, multiplication ...).
<br><br>
Lisp aggregated and evolved all the important techniques used in programming languages today:
stepwise code optimization, just-in-time compilation, macro expansion, polymorphism, object systems, lexical and dynamic scoping, tail recursion, garbage collection, parameter passing modes,
exception handling, etc.
<br><br>
Lisp was developed by discovery, rather than predefinition, and each successful experiment led to another language "standard feature".
Eventually this led to overstuffing, as witnessed by Guy Steele's 1000-page tome *Common Lisp*.
With a specification that size, many partially compatible subsets emerged, losing some of the *Common* in *Common Lisp*.
In reaction to the size of Common Lisp, an effort was made to extract the essential behaviors of Lisp into a simpler package, which became *Scheme*.
<br><br>
The current (2025) *Scheme* language specifications are known as *Scheme R5RS and R7RS*.  All implemented LambLisp features conform to these specifcations.
</div>

# LambLisp Architecture

## High-level AST generation
<div style="wrap">
<br>
LambLisp's implements interpretation of Lisp at a high level; each of the functions in the *Scheme RxRS* specifications is implemented in C++, rather than a lower-level bytecode system.
This reduces the "semantic distance" between the language specification and the control application,
by reducing the number of steps to be interpreted between source code intake and its  execution in C++.
<br><br>
LambLisp uses a "direct-connect" technique, in which high-level Lisp operations (implemented in C++) are executed through pointers directly embedded into Lisp objects.
These objects correspond directly to high-level features in the language specification (define, lambda, etc).
All of the LambLisp language primitives are implemented this way, and it is possible to add additional LambLisp functions implemented in C++.
They are treated as any other first-class function.
<br><br>
\image html LambLisp_BlockDiagram.svg
<br>
When source code is read in, the *reader* front end of the interpreter tokenizes the text, parses the the resulting code, and constructs an *abstract syntax tree (AST)*.
The AST is the executable version of the code, an ordered collection containing all the parts and their relationships.
Because the resulting executable is a series of direct links, the AST can be traversed rapidly without a lookup table,
and because the tree end nodes are high-level functions (rather than low-level bytecodes), the result is a fast interpreter.
</div>

## Memory management

### Overview
<div style="wrap">
Memory reuse was recognized as a challenge right from the early days of programmable computers.  Most computational results are intermediate;
once calculated, they are quickly combined with other results and no longer required.
*Garbage collection* is the pejorative term for activities related to the identification and reclamation of unused memory.
Every programming language must have a strategy for reusing the memory space that no longer holds useful results.
<br><br>
There are 4 main ways to allocate memory: 1) static allocation, 2) stack allocation, 3) heap allocation, and 4) "manually".
<br><br>
Static allocation is done once before the program starts.  The memory remains available for use by the application for the life of the program.
If this memory is to be used for multiple purposes, the processes involved in reuse must be done "manually" by the running application.
<br><br>
Stack allocation is what C programmers experience as "local variables".  When a function is called, a new block is allocated from the top of the execution stack.
These blocks are interwoven with the return address of the calling functions.
When a function returns, the local storage block is popped off and the calling function's context is now at top of stack.
The total available stack space is usually small, often just 4k or 8k bytes.
<br><br>
The memory not allocated statically or to the stack is referred to as the "heap".
C and C++ programmers must manage this space manually, using the alloc()/free() or new/delete functions, and employ bespoke tracking mechanisms to
discriminate used space from unused.
<br><br>
Many other languages allocates large blocks from the heap, and manage the interior allocation of those blocks according tto the requirements of the language.
Among these languages are Python, Java, JavaScript, Lua, and of course Lisp.
</div>

### Garbage collection

<div style="wrap">
The literature on garbage collection is broad and deep, with a great many features identified among the various possible algorithms:
copying vs. not, compacting vs not, support for circular structures, and other relevant characteristics.
<br><br>
In a real-time control system, the discriminating factor for implementing a garbage collector is that it can perform its function incrementally,
rather than interrupt control of the process while garbage collection occurs.
The control software must implement a limit on the length of time that the process is unmonitored while GC is occurring.
<br><br>
LambLisp's memory reuse strategy relies on these key papers:
- Dijkstra 1978 describes the "tricolor abstraction" and proves the correctness of incremental garbage collection
- Wilson 1992 summarizes uniprocessor GC techniques and introduces a "stack" instead of a fifth color.
<br><br>
\image html "LambLisp_GC_Cycle.svg"
</div>


## Scalability
<div style="wrap">

Scalability is a matrix; one axis is simply "scale up or down".  The other consists of discrete modes or techniques for scaling.
LambLisp has several modes of scaling up and down.
</div>

### Scaling up by adding new fundamental features
<div style="wrap">

LambLisp also has a clean, simple interface to C++ code.  All the LambLisp native functions have the same signature.
As part of the control application, additional native features can be added and will run at full C++ speed.
These additions will commonly perform some type of hardware abstraction, and will implement control of hardware devices.
<br><br>
For example, LambLisp implements the *digitalWrite* function this way:
</div>

```C++
Sexpr_t mop3_hal_digitalWrite(Lamb &lamb, Sexpr_t sexpr, Sexpr_t env_stack)
{
  ME("::mop3_hal_digitalWrite()");			//System macro for exception unwinding
  Sexpr_t sx_pin = lamb.car(sexpr);			//Get the pin # in S-expression form
  Sexpr_t val = lamb.cadr(sexpr);			//Get the value in S-expression form
  mop3_typecheck(sx_pin, T_INT);			//System macro to verify S-expression type before trusting contents

  //Extract the C-level integer pin # and write the value
  digitalWrite(sx_pin->as_Int_t(), val != HASHF);	//No check on value; in Scheme, any value "not false" is "true"
  return val;
}
```
To use this C++ function from within LambLisp to set pin 38 HIGH and then LOW:
```Lisp
(digitalWrite 38 #t)
(digitalWrite 38 #f)
```
### Scaling down by removing unneeded functions
<div style="wrap">

At build time, LambLisp allows fine-grained control of the system functions that are included in the application image.
<br><br>
For example, LambLisp supports all the math functions described in the *Scheme RxRS* specifications, and by default all are included in the build.
Functions that are not needed can be commented out in the source code and will not be linked into the control application.
<br><br>
This same approach can be used for other groups of function, such as ports and strings.
These groups of functions are completely defined in the spec to provide a full solution in their respective domains,
but in practice each application uses only a subset of the available capability.
<br><br>
As another example, few embedded control applications will need the full set of case-independent string operators, and they are easy to add in Lisp later if needed,
These functions are candidates for implementation in Lisp and loading at runtime, rather than including them in every binary application image.
<br><br>
This scalability feature provides a means to reduce the application size.
</div>

### Scaling up or down by controlling memory allocation.
<div style="wrap">

LambLisp has an adaptable real-time garbage collection implementation that can be scaled in several ways.
At every *cons* operation, the GC has an opportunity to run, and may mark or sweep cells at that time.
<br><br>
| GC parameter     | Function                                                     |
|------------------|--------------------------------------------------------------|
| GC mark quantum  | # cells to mark during each mark increment (default 256-ish) |
| GC sweep quantum | # cells to mark during each sweep increment (default 64-ish) |
| Cell block size  | # cells per block (default 4k)                               |
| Max cell blocks  | maximum # of blocks to allocate (default 32)                 |
| GC % threshold   | Increase memory until threshold reached (or cap)             |

The GC quanta parameters determine how many mark or sweep operations to do during each increment.
A smaller number results in shorter GC interruptions, but lower total throughput.
The cell block size is not critical, but the minimum system setup needs 4k cells, and so
4k is a natural default for this parameter.
<br><br>
The maximum number of cell blocks can be chosen to tune the amount of time spent in GC.
The amount of time spent marking depends only on the size of the AST, and not the number of cells in the population.
The amount of time spent sweeping is just the opposite: the entire cell population must be swept, regardless of the AST size.
<br><br>
With more cells in the population, more cells are swept and reclaimed with each GC mark/sweep cycle, because the fixed cost of marking
the cells in the AST is amortized over the entire cell population.
</div>
#### LambLisp adaptive tuning
<div style="wrap">
<br>
LambLisp implements several adaptive tuning mechanisms in the GC implementation.
<br><br>
One of the principal concerns is the amount of CPU time devoted to garbage collection.
In LambLisp, the percentage of time used in GC is the control point for determining whether to expand the cell population.  If the 
percentage time in GC is above the configured threshold, then an additional cell block is added.  As long as the threshold is exceeded,
new blocks will be added up to the configured maximum.
<br><br>
While in the GC marking phase used cells are being identifed, while conversely no unused cells are being identified.
During this time all newly allocated cells come directly from the free list.
<br><br>
That means that the free list must contain enough cells to service all possible allocaton requests during the mark phase.
Estimating the size of the AST provides a benchmark for setting the required size of the free list.  Performing this exercise
provides interesting and useful results.
<br><br>
At the end of marking, the cells on the free list are "excess".
The system could have spent a less time on GC and still had enough cells on the free list to satisfy demand.  Potentially the mark quantum could be reduced.
On the other hand, if the free list runs empty while marking is still in progress, then more time should have been spent on marking,
and the mark quantum should be increased.
<br><br>
At the end of sweeping, the number of free cells is known, and all the non-free cells are potentially part of the AST.
That is the inital estimate.  When cells are issued from the free list, they add to the potential size of the AST.
<br><br>
When the cells on the free list plus the estimated AST size fall amount to half of the available memory, a mark phase must be started.
Because the mark quantum is large relative to the issuance, we are guaranteed to finish the mark cycle
before the free list is exhausted (remembering that every cell issuance is a GC opportunity).
<br><br>
It is possible to dynamically adjust the GC mark quantum in real time, based on the free memory available.  By constantly updating the estimate of AST size,
we can use less than half of the memory as reserve immediately after GC marking, until continued issuance increases the AST size estimate.
This will result in a sawtooth-shaped performace profile that will improve throughput if sufficient memory is available, but will always end when half the memory is free.
<br><br>
In cases where "excess" memory is not available (i.e., more than half) then the LambLisp GC implementation will anyway hover at the 50% mark.
In other words, the heuristic is this: Continuously mark and sweep, although sweeping can be skipped if the free list is more than half of the cell population.
If ever during the mark phase a shortage of cells is detected, then increase the mark quantum up to a limit.
<br><br>
Scaling the GC sweep quantum is also possible.  At each GC opportunity, there are two responsibilities.
Primarily, ensure that there is a free cell available to be issued.
Second, ensure that progress is made on GC, by sweeping some cells and reclaiming those that are knownn to be unused.  As long as the GC process stays ahead
of cell consumption, the sweep quantum is not critical.  Keeping it small reduces the GC pause time, at the cost of some additional overhead.  A larger value
will increase throughput but with longer time spent at each increment.
<br><br>
On a 240 MHz ESP32, the mark operation takes about 1.5 us, while the sweep takes about 0.5 us, per cell.  To maintain uniform GC pauses the ratio of
mark quantum to sweep quantum should be about the same as the inverse, 1:3.  This is also not critical, as the sweep process typically frees cells in groups,
and many sweep passes may be skipped while the free list is consumed.
</div>

### Scaling by incremental compilation
<div style="wrap">

Once constructed, ASTs are subject to further optimization for speed at the expense of size.
ASTs are compounds of native Lisp functions (implemented in C++), as well as those composed in Lisp.
By replacing Lisp functions with *macros*, expressions can be expanded once into their final forms, replacing functions composed in Lisp with their equivalent in Lisp primitives.
By doing that replacement selectively, it is possible to tune the application while expanding only those portions that improve performance.
</div>

### Scaling by using remote memory

<div style="wrap">
Because programs are ASTs, and ASTs are data structures, it is possible to store programs, delete them from memory, and retrieve them later.
This approach provides a *virtual memory* capability, which is useful in many circumstances.

| Virtual memory advantages                                                                |
|------------------------------------------------------------------------------------------|
| Startup code that is used once can afterward be purged and the space reclaimed.          |
| Algorithms may be chosen and downloaded at runtime, depending on circumstance.           |
| New algorithms may be introduced in the field, while the process is still under control. |

</div>

## Cell memory model

<div style="wrap">
At the lowest level, LambLisp depends on a *cell* structure in 3 words of computer memory.
The Lisp *car* and *cdr* are represented in 2 words of the cell, and a third word contains information about the cell, such as
the cell type and its garbage collection state.
<br><br>
Here are the layouts for the most common *LambLisp Cell* data types.
<br><br>

| Cell type                   | Word 0       | Word 1      | Word 2                 |
|-----------------------------|--------------|-------------|------------------------|
| Pair types                  | type + flags | car         | cdr                    |
| Boolean, character, integer | type + flags | value       | reserved               |
| Real numbers                | type + flags | real part 1 | real part 2            |
| Vector, string, bytevector  | type + flags | length      | pointer to native type |
| Symbol                      | type + flags | hash        | pointer to string      |
</div>

# Benefits of the Interpreter-oriented architecture.
<div style="wrap">

| Benefit                                                                                                                                                                                                                     |
|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| The interpreter is compact, allowing more memory for the high-level control application.                                                                                                                                    |
| LambLisp's direct-connect parse tree execution provides high performance.                                                                                                                                                   |
| The interpreter is the entire Lisp virtual machine runtime, already compiled from efficient C++. The Lisp runtime would anyway be included as the functional core of any compiled application.                              |
| No need for full over-the-air updates and reboot.  Download new code into the storage system and it will execute on next loop().                                                                                            |
| Lisp code can be stored in file system, dynamically loaded & purged, reloaded etc, providing a virtual memory capability.                                                                                                   |
| No off-board compiler/linker are required to run LambLisp code.  Just download the source code to the device.                                                                                                               |
| LambLisp's incremental, adaptive garbage collector recycles memory automatically and predictably, without worst-case pauses.                                                                                                |
| The LambLisp virtual machine, written in C++, is much smaller than an all-C++ application.  This means there are fewer low-level features, fewer bugs, requiring less memory and fewer updates than a full C++ application. |
| Arduino compatibility, with easy addition of extensions.                                                                                                                                                                    |
| No "foreign functions" are required as in other Lisp implementations.  Functions written in C++ can be natively incorporated into the Lisp execution environment.                                                           |
| Provides a basis for macro-based incremental & just-in-time compilation                                                                                                                                                     |

</div>

## Other Scheme implementations
<div style="wrap">
This project was started after a review of available options for run-time languages to augment C++ embedded applications, and the likely candidates were all on the Lisp/Scheme spectrum.
Some of the existing implementation that were examined include: TinyScheme, ulisp, microlisp, picolisp, T, L, Pre-Scheme, LispBM, chicken, bigloo, racket, chibi and others.
Plug those into your favorite search engine to find even more.
<br><br>
General objections to existing Lisp/Scheme implementations were:
<br>
|                                                                                                                                         |
|-----------------------------------------------------------------------------------------------------------------------------------------|
| Required off-board compilation.                                                                                                         |
| Out of scale for a microprocessor.                                                                                                      |
| Non-portable hardware tricks (e.g., using the bottom bits of addresses as flag bits).                                                   |
| Non-Scheme lisps (e.g., no lexical scoping or tail recursion).                                                                          |
| Bad fit on the interpret<->compile spectrum (e.g, low-level interpeter or huge compiler).                                               |
| Lisp-on-Python, Lisp-on-JavaScript, and other such proofs.                                                                              |
| Garbage collection: Real-time applications must avoid pauses, so stop-the-world, reference counting and generational GC cannot be used. |
</div>

# Why is it called LambLisp instead of Scheme?
<div style="wrap">
During the 1960s, a great many of the desired and desirable behaviors of programming languages were still being worked out.
While Fortran was used heavily for numeric computation, *lexical scoping* (which C programmers experience as "local variables") arose in the Algol language,
and later "message passing" systems (primarily *SmallTalk*) resulted in the "tail recursion" technique and interactive computing.
<br><br>
*LISP* provided a significant abstraction of the computation process.
By operating primarily on addresses rather than values, *Lisp* provided a method for creating new and arbitrary data structures.
A great deal of research and discussion was expended on functions and their abstract properties (fexprs, nlambdas and the rest).
With these tools and techniques, *Lisp* provided a facility for reasoning, not just calculating.
<br><br>
These and other ideas came together in *Scheme*, which was designed in the 1970s to integrate the theoretical advances and practical understanding that had occurred over the previous 15 years.
<br><br>
*Scheme* went through several iterations and the usual technical committee sausage-making process, resulting in two main specifications today, known as R5RS and R7RS.
<br><br>
There has always been tension between *Scheme* minimalists, who prefer to specify a small set of language primitives, and utilitarians, who prefer a language that is useful in a practical sense.
<br><br>
This had led to the sadly comical state where the R7RS specification had to be divided into R7RS-small and R7RS-large,
with R7RS-small already being significantly larger tha R5RS specification.
<br><br>
There are few *Scheme* implementations that are completely conformant to R5RS or R7RS.
Both specs allow plenty of leeway for alterative behaviors, for example in the implementation of macros, or the results of invalid computations.
<br><br>
For some *Scheme* enthusiasts, the function *call-with-current-continuation* is the *sine qua non* of *Scheme* implementations,
so that argues against labeling this system *Scheme*.  The latest thinking seems to be that this feature is actually not as great in practice as in theory, and may
be removed (or optionalized) in some future R8RS specification.
<br><br>
Likewise, *LambLisp* has a simple macro system based on the *nlambda* feature of InterLisp, while the RxRS specs call for a macro system based on the language primitive *syntax-rules*.
This seems to be in flux with *syntax-case* proposed as an improved alternative to *syntax-rules*.
<br><br>
Because *nlambda* is sufficient for implementation, *syntax-rules* is parked.  Future work on macros will focus on their use for just-in-time compilation.
<br><br>
The differences in support for *call-with-current-continuation* and *syntax-rules* argue against calling this language *Scheme*.
<br>
</div>

# LambLisp Compatibility Matrix

<div style="wrap">
\htmlinclude LambLisp_Compatibility_Matrix.html
</div>
