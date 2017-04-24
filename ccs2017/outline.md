Motivation for new CT language

- CT code is hard to read/write/reason about
   - What's the evidence for this?
     - github commit logs of bugs fixed in CT code
     - 2016 openSSL padding oracle; missing check
     - hard to reason about function calls: what if function that you call in
       your bit mask is not constant time? (find example where function is
       called.)
- CT code is not leveraging hardware
  - people use ASM directly; this is error prone
  - In C code you use bitmask when HW gives you cmove & bswap
    - Q: Do the macros save registers to use them?
    - TODO: call C/asm functions from FaCT
  - Add with carry?



Need new language
- High-level, less error prone/correct by construction; type system stops you
  from doing bad stuff
- Interop with C
- Exposes features that map well to hardware
